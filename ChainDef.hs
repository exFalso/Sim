{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module ChainDef where

import Node
import Distribution

import Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity

type Graph = Map.Map NodeId AnyNode

newtype ChainDef a = ChainDef { unChainDef :: StateT NodeId (StateT Graph Identity) a }
                   deriving (Monad)

runChainDef :: ChainDef a -> (a, Graph)
runChainDef = (\((a, _), b) -> (a, b)) 
              . runIdentity
              . flip runStateT Map.empty
              . flip runStateT 2 -- 0 and 1 reserved for source/sink
              . unChainDef

newId :: ChainDef NodeId
newId = do
  ChainDef get >>= \n -> ChainDef (put (n + 1)) >> return n

getGraph :: ChainDef Graph
getGraph = ChainDef (lift get)

putGraph :: Graph -> ChainDef ()
putGraph = ChainDef . lift . put

trans :: (Distribution d, SrcNode src, DstNode dest) => src -> dest -> d -> ChainDef ()
trans source target distr = do
  graph <- getGraph
  let newNode = addTrans (MkDestNode target, MkDistr distr) (MkSourceNode source)
  putGraph $ Map.insert (nodeId (nodeInfo newNode)) (toAny newNode) graph

createNode :: NodeClass n => (NodeId -> n) -> ChainDef n
createNode newNode = do
  i <- newId
  graph <- getGraph
  let node = newNode i
  putGraph $ Map.insert i (toAny node) graph
  return node
  

createInter :: Logged -> ChainDef Inter
createInter l = createNode (\i -> Inter (NodeInfo i l) [])

createSink :: (Distribution d) => Logged -> d -> ChainDef Sink
createSink l d = createNode (\i -> Sink (NodeInfo i l) (MkDistr d))

createSource :: Logged -> ChainDef Source
createSource l = createNode (\i -> Source (NodeInfo i l) [])

