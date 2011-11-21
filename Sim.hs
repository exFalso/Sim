{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sim where

import SimChainDef
import Random
import Distribution
import ChainDef
import Node
import EventResult
import SimNode

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


-- | SimT is a transformer representing a simulation
newtype SimT m a = Sim
                   { unSim :: ReaderT (ChainDef ())
                              (ReaderT (Channel MasterEvent)
                               (ReaderT [ResultVar] m)) a }
                 deriving (Monad, MonadIO)

instance MonadTrans SimT where
  lift = Sim . lift . lift . lift

askMaster :: MonadIO m => SimT m (Channel MasterEvent)
askMaster = Sim . lift $ ask

askResVars :: MonadIO m => SimT m [ResultVar]
askResVars = Sim . lift . lift $ ask

tellMaster :: Channel MasterEvent -> Double -> MasterEvent -> IO ()
tellMaster master time event = atomically $ writeTChan master (time, event)

runSimT :: MonadIO m => SimT m a -> ChainDef () -> m a
runSimT simulation chainDef = do
  (masterChannel, resultVars, initGraph) <- liftIO $ initialiseSimGraph chainDef
  let graph = wireUpIncomingSimGraph initGraph
  let ((), simpleGraph) = runChainDef chainDef
  let rng = R.getStdGen
  liftIO $ startThreads graph simpleGraph rng
  ret <- flip runReaderT resultVars
         . flip runReaderT masterChannel
         . flip runReaderT chainDef
    $ unSim simulation
  liftIO $ tellMaster masterChannel 0 EventShutdown
  return ret

startThreads :: SimGraph -> Graph -> IO R.StdGen -> IO R.StdGen
startThreads graph simpleGraph rng =
  (\f -> Map.foldWithKey f rng graph) $
    \nid initInfo lastGen -> do
      gen <- lastGen
      anyNode <- return $ 
                 fromJust (Map.lookup nid simpleGraph)
      let (aGen, bGen) = R.split gen
      let f a = forkIO (runRandomT (runNode a initInfo) aGen)
      foldAny f f f anyNode
      return bGen

flushAt :: MonadIO m => Double -> SimT m [(NodeId, [EventResult])]
flushAt time = do
  master <- askMaster
  liftIO $ tellMaster master time EventFlush
  resVars <- askResVars
  liftIO . flip mapM resVars $ \(nid, tVar) -> atomically $ do
    res <- readTVar tVar
    case res of
      Nothing -> retry
      Just results -> return (nid, results)
