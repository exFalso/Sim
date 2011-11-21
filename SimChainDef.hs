{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | semi markov chain def for wiring up TChans using the pure (ChainDef) chain def
module SimChainDef where

import Node
import ChainDef
import EventResult
import Random

import System.Random
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Heap
import Data.Maybe
import qualified Data.Map as Map

-- every node has at least one incoming node for the master thread to control when to flush results or to shut down
-- they will all block unless there is a master event scheduled

data MasterEvent = EventShutdown      -- shut down thread
                 | EventFlush         -- flush results

data SlaveEvent = TimeDummy    -- to set time when no transition happened
                | Event

type Channel a = TChan (Double, a)
type OutNodes = [OutTrans]    -- clocks always reset therefore we need to poke every distribution

type SimGraph = Map.Map NodeId InitNodeInfo

data InitNodeInfo = InitNodeInfo AnyNode [InTrans] [OutTrans] ResultVar

-- | an incoming transition
data InTrans = Master (Channel MasterEvent)
             | Slave NodeId (Channel SlaveEvent)

-- |an outgoing transition
data OutTrans = OutTrans Transition (Channel SlaveEvent) -- NodeId could be changed to a function of some kind to make the simulation GSMP

type ResultVar = (NodeId, TVar (Maybe [EventResult])) -- master blocks on Nothing


mmapM :: (Monad m, Ord k) => (a -> m b) -> Map.Map k a -> m (Map.Map k b)
mmapM f mp = (\g -> Map.foldWithKey g (return Map.empty) mp) $ \k a mm -> do
  b <- f a
  m <- mm
  return (Map.insert k b m)

-- |create channels but dont wire up the incoming transitions yet (except master)
initialiseSimGraph :: ChainDef () -> IO (Channel MasterEvent, [ResultVar], SimGraph)
initialiseSimGraph chainDef = do
  let ((), graph) = runChainDef chainDef
  masterChannel <- newTChanIO
  (doneGraph, resultVars) <- flip runStateT []
                             . flip mmapM graph $ \node -> do
    resultChan <- liftIO $ newTVarIO Nothing -- TODO check if it needs to be logged
    let resultVar = (nodeId (nodeInfo node), resultChan)
--    lift . putStrLn $ "WAT: " ++ show (nodeId (nodeInfo node))
    modify (resultVar :)
    dupMaster <- liftIO $ atomically (dupTChan masterChannel)
    outgoing <- liftIO . flip mapM (transitions node) $ \tran -> do
      outChannel <- newTChanIO
      return (OutTrans tran outChannel)
    return (InitNodeInfo node [Master dupMaster] outgoing resultVar)
  return (masterChannel, resultVars, doneGraph)
    
-- | wire up incoming transitions
wireUpIncomingSimGraph :: SimGraph -> SimGraph
wireUpIncomingSimGraph graph = (\f -> Map.foldWithKey f graph graph)
                               $ \key (InitNodeInfo fromNode _ outgoing _) graph -> runIdentity $ do
  foldM (\gr (OutTrans (toNode, _) outChannel) -> do
            let toId = nodeId (nodeInfo toNode)
            let (InitNodeInfo _ inco outg resultVar) = fromJust (Map.lookup toId gr)
            return $ Map.insert toId
              (InitNodeInfo (toAny toNode)
               (Slave
                (nodeId (nodeInfo fromNode))
                outChannel : inco
               ) outg resultVar) gr) graph outgoing
