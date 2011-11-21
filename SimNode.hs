{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimNode where

import SimChainDef
import EventResult
import Random
import Distribution
import Node

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Cont
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe

import qualified System.Random as R
import qualified Data.Heap as Heap
import qualified Data.Map as Map


class SimNodeClass n where
  runNode :: MonadIO m => n -> InitNodeInfo -> RandomT m ()

instance SimNodeClass Sink where
  runNode (Sink nodeInf distr) (InitNodeInfo node ints outts resVar) = do
    inNodes <- buildInNodes ints
    runSimNodeT (simSink distr) resVar inNodes outts

  -- TODO: handle errors (like empty outgoing transitions)
instance SimNodeClass Source where
  runNode (Source nodeInf _) (InitNodeInfo node ints outts resVar) = do
    inNodes <- buildInNodes ints
    runSimNodeT (simSource (nodeId nodeInf)) resVar inNodes outts

instance SimNodeClass Inter where
  runNode (Inter nodeInf _) (InitNodeInfo node ints outts resVar) = do
    inNodes <- buildInNodes ints
    runSimNodeT simInter resVar inNodes outts

buildInNodes :: MonadIO m => [InTrans] -> m InNodes
buildInNodes intts = (\g -> foldM g Heap.empty intts) $ \heap inTrans -> liftIO $
  case inTrans of
    Master masterChannel -> do
      -- will block if master hasnt sent anything
      (time, event) <- atomically (readTChan masterChannel)
      return (Heap.insert (OrdMaster time event masterChannel) heap)
      -- wait for all slaves to send an event (isolated Inter slave should send +INFINITY!!!)
    Slave nid slaveChannel -> do
      (time, event) <- atomically (readTChan slaveChannel)
      return (Heap.insert (OrdSlave time nid event slaveChannel) heap)
      
-- |SimNodeT is a transformer representing one node's lifetime
newtype SimNodeT m a = SimNode 
                       { unSimNode :: ReaderT ResultVar  -- results
                                      (StateT InNodes    -- incoming transitions (heap)
                                       (ReaderT OutNodes -- outgoing transitions
                                        (StateT [EventResult]
                                         (StateT Double   -- "current time"
                                          (RandomT m      -- rng
                                          ))))) a }
                     deriving (Monad, MonadIO)
  
instance MonadTrans SimNodeT where
  lift = SimNode . lift . lift . lift . lift . lift . lift

instance Monad m => MonadRandom (SimNodeT m) where
  nextRandom = SimNode nextRandom

runSimNodeT :: MonadIO m => SimNodeT m a -> ResultVar -> InNodes -> OutNodes -> m a
runSimNodeT sim res inn outn = runRandomTStd -- TODO: let user set seed
                               . liftM fst . flip runStateT 0 -- TODO: let user set starting time
                               . liftM fst . flip runStateT []
                               . flip runReaderT outn
                               . liftM fst . flip runStateT inn
                               . flip runReaderT res
                               $ unSimNode sim

-- U(0,1)
uniRandom :: (Monad m) => SimNodeT m Double
uniRandom = return . snd . properFraction =<< nextRandom

pokeR :: (MonadIO m) => Distr -> SimNodeT m Double
pokeR (MkDistr d) = uniRandom >>= return . poke d
  -- uni <- uniRandom
  -- let ret = poke d uni
  -- liftIO . putStrLn $ show (uni, ret)
  -- return ret

getInNodes :: (Monad m) => SimNodeT m InNodes
getInNodes = SimNode . lift $ get

putInNodes :: (Monad m) => InNodes -> SimNodeT m ()
putInNodes = SimNode . lift . put

askOutNodes :: (Monad m) => SimNodeT m OutNodes
askOutNodes = SimNode . lift . lift $ ask

getTime :: (Monad m) => SimNodeT m Double
getTime = SimNode . lift . lift . lift . lift $ get

setTime :: (Monad m) => Double -> SimNodeT m ()
setTime = SimNode . lift . lift . lift . lift . put

modifyTime :: (Monad m) => (Double -> Double) -> SimNodeT m ()
modifyTime = SimNode . lift . lift . lift . lift . modify

plusTime :: (Monad m) => Double -> SimNodeT m ()
plusTime dt = modifyTime (dt +)

setMaxTime :: (Monad m) => Double -> SimNodeT m ()
setMaxTime t = getTime >>= \c -> if c > t then return () else setTime t

askResVar :: (Monad m) => SimNodeT m ResultVar
askResVar = SimNode ask

getResults :: (Monad m) => SimNodeT m [EventResult]
getResults = SimNode . lift . lift . lift $ get

putResults :: (Monad m) => [EventResult] -> SimNodeT m ()
putResults = SimNode . lift . lift . lift . put

modifyResults :: (Monad m) => ([EventResult] -> [EventResult]) -> SimNodeT m ()
modifyResults = SimNode . lift . lift . lift . modify

data OrdInTrans = OrdMaster Double MasterEvent (Channel MasterEvent)
                | OrdSlave Double NodeId SlaveEvent (Channel SlaveEvent)

instance Eq OrdInTrans where
  a == b = timeOf a == timeOf b
    where
      timeOf (OrdMaster t _ _) = t
      timeOf (OrdSlave t _ _ _) = t

instance Ord OrdInTrans where
  a < b = timeOf a < timeOf b
    where
      timeOf (OrdMaster t _ _) = t
      timeOf (OrdSlave t _ _ _) = t

type SimNodeContT m = ContT () (SimNodeT m)

waitForNextTrans :: MonadIO m => OrdInTrans -> SimNodeT m OrdInTrans
waitForNextTrans (OrdMaster _ _ masterChannel) = do
  (time, event) <- liftIO $ atomically (readTChan masterChannel)
  return (OrdMaster time event masterChannel)
waitForNextTrans (OrdSlave _ nid _ slaveChannel) = do
  (time, event) <- liftIO $ atomically (readTChan slaveChannel)
  return (OrdSlave time nid event slaveChannel)
  

withInTrans :: (MonadIO m) => (OrdInTrans -> SimNodeContT m a) -> SimNodeContT m a
withInTrans f = do
  inNodes <- lift getInNodes
  let (inTrans, restHeap) = fromJust (Heap.view inNodes)
  ret <- f inTrans
  nextTrans <- lift $ waitForNextTrans inTrans
  lift $ putInNodes (Heap.insert nextTrans restHeap)
  return ret

_BUFFER_SIZE = 64               -- because why not

_SOURCE = 0                     -- reserved NodeIds
_SINK = 1

flushResults :: (MonadIO m) => SimNodeT m ()
flushResults = do
  (_, resVar) <- askResVar
  results <- getResults
  putResults []
  liftIO . atomically $ do
    readTVar resVar >>= check . isNothing
    writeTVar resVar (Just results)

storeResult :: (MonadIO m) => Double -> NodeId -> NodeId -> SimNodeT m ()
storeResult arrTime fromNode toNode = do
  currTime <- getTime
  modifyResults (EventResult fromNode toNode arrTime currTime :)

  -- may introduce deadlock, dont use for now
-- waitUntilEmpty :: (MonadIO m) => SimNodeT m ()
-- waitUntilEmpty = do
--   outChans <- askOutNodes >>= return . map (\(OutTrans _ chan) -> chan)
--   mapM_ (\tChan -> liftIO . atomically $ check =<< isEmptyTChan tChan) outChans

withShutdown :: (Monad m) => (SimNodeContT m b -> SimNodeContT m ()) -> SimNodeT m ()
withShutdown f = flip runContT id . callCC $ \sd -> f (sd (return ()))  >> return (return ())

simSink :: (MonadIO m) => Distr -> SimNodeT m ()
simSink distr = withShutdown $ \shutdown -> forever $ do -- lol
  withInTrans $ \inTrans -> case inTrans of
    OrdMaster _ EventShutdown _ -> do
      lift flushResults
      shutdown
    OrdMaster time EventFlush _ -> lift $ do
      flushResults
      setMaxTime time
    OrdSlave time nid Event _ -> lift $ do
      setMaxTime time
      dt <- pokeR distr
      plusTime dt
      storeResult time nid _SINK
    OrdSlave time _ TimeDummy _ -> lift $ do
      setMaxTime time

simInter :: (MonadIO m) => SimNodeT m ()
simInter = withShutdown $ \shutdown -> forever $ do
  withInTrans $ \inTrans -> case inTrans of
    OrdMaster _ EventShutdown _ -> do
      lift flushResults
      shutdown
    OrdMaster time EventFlush _ -> lift $ do
      flushResults
      setMaxTime time
    OrdSlave time fromId Event _ -> lift $ do
      setMaxTime time
      ((chan, toId, dt), rest) <- getOutNode
      plusTime dt
      currTime <- getTime
      liftIO . atomically $ writeTChan chan (currTime, Event)
      -- set times of other neighbouring nodes
      liftIO . mapM_ (\chan -> atomically $ writeTChan chan (currTime, TimeDummy)) $ rest
      storeResult time fromId toId
    OrdSlave time _ TimeDummy _ -> lift $ do
      setMaxTime time

simSource :: (MonadIO m) => NodeId -> SimNodeT m ()
simSource nid = withShutdown $ \shutdown -> forever $ do
  withInTrans $ \inTrans -> case inTrans of
    OrdMaster time EventShutdown _ -> do
      lift $ sendUntil time
      lift flushResults
      shutdown
    OrdMaster time EventFlush _ -> lift $ do
      sendUntil time
      flushResults
      setMaxTime time
    OrdSlave _ _ _ _ -> error "OrdSlave in Source???"
  where sendUntil time = timeout time $ do
          startTime <- getTime
          ((chan, toId, dt), rest) <- getOutNode
          plusTime dt
          currTime <- getTime
          liftIO . atomically $ writeTChan chan (currTime, Event)
          -- set times of other neighbouring nodes
          liftIO . mapM_ (\chan -> atomically $ writeTChan chan (currTime, TimeDummy)) $ rest
          storeResult startTime 0 toId


timeout :: (MonadIO m) => Double -> SimNodeT m () -> SimNodeT m ()
timeout deadline what = do
  currTime <- getTime
  if currTime > deadline then return () else what >> timeout deadline what

-- |returns (channel, delta time), the channel to be chosen as the next outgoing one
getOutNode :: (MonadIO m) => SimNodeT m ((Channel SlaveEvent, NodeId, Double), [Channel SlaveEvent])
getOutNode = do
  outNodes <- askOutNodes
  list <- flip mapM outNodes $ \(OutTrans (node, distr) channel) -> do
    dt <- pokeR distr
    return (channel, nodeId (nodeInfo node), dt)
  let (rest, val) = flip minWith list $ \(_, _, dt1) (_, _, dt2) -> dt1 < dt2
  return (val, map (\(ch, _, _) -> ch) rest)

minWith less l = (\(f, m) -> (f [], m)) $ foldl (\(f, a) b -> if less a b
                                                              then (f . (b :), a)
                                                              else (f . (a :), b)) (id, head l) (tail l)
  
type InNodes = Heap.MinHeap OrdInTrans -- we need to keep all incoming events to determine their actual order, always pop minimal

