{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Node where

import Distribution

import Control.Monad.IO.Class

-- | "flag" passed from user
data Logged = Logged { logName :: String }
            | Unlogged
            deriving (Show)
class (Show n, NodeClass n) => SrcNode n where
  addTrans :: Transition -> n -> n
class (Show n, NodeClass n) => DstNode n where

type NodeId = Integer

type Transition = (DestNode, Distr)

data NodeInfo = NodeInfo { nodeId :: NodeId
                         , nodeLogged :: Logged }
              deriving (Show)

data Source = Source NodeInfo [Transition]
            deriving (Show)
instance SrcNode Source where
  addTrans t (Source inf ts) = Source inf (t : ts)

data Sink = Sink NodeInfo Distr
          deriving (Show)
instance DstNode Sink where

data Inter = Inter NodeInfo [Transition]
          deriving (Show)
instance SrcNode Inter where
  addTrans t (Inter inf ts) = Inter inf (t : ts)
instance DstNode Inter where
  
data AnyNode = AnyInter Inter
             | AnySource Source
             | AnySink Sink
             deriving (Show)

foldAny :: (Inter -> a) -> (Source -> a) -> (Sink -> a) -> AnyNode -> a
foldAny a b c d = case d of
  AnyInter x -> a x
  AnySource x -> b x
  AnySink x -> c x

data SourceNode where
  MkSourceNode :: (NodeClass n, SrcNode n) => n -> SourceNode
deriving instance Show SourceNode

instance SrcNode SourceNode where
  addTrans t (MkSourceNode n) = MkSourceNode (addTrans t n)
  
data DestNode where
  MkDestNode :: (NodeClass n, DstNode n) => n -> DestNode
deriving instance Show DestNode
  

class (Show n) => NodeClass n where
  nodeInfo :: n -> NodeInfo
  transitions :: n -> [Transition]
  toAny :: n -> AnyNode

instance NodeClass Sink where
  nodeInfo (Sink n _) = n
  transitions _ = []
  toAny = AnySink

instance NodeClass Source where
  nodeInfo (Source n _) = n
  transitions (Source _ ts) = ts
  toAny = AnySource

instance NodeClass Inter where
  nodeInfo (Inter n _) = n
  transitions (Inter _ ts) = ts
  toAny = AnyInter

instance NodeClass AnyNode where
  nodeInfo a = foldAny nodeInfo nodeInfo nodeInfo a
  transitions a = foldAny transitions transitions transitions a
  toAny = id

instance NodeClass DestNode where
  nodeInfo (MkDestNode n) = nodeInfo n
  transitions (MkDestNode n) = transitions n
  toAny (MkDestNode n) = toAny n

instance NodeClass SourceNode where
  nodeInfo (MkSourceNode n) = nodeInfo n
  transitions (MkSourceNode n) = transitions n
  toAny (MkSourceNode n) = toAny n
