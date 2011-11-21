{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Distribution where

class (Show d) => Distribution d where
  -- | give a value provided an x with uniform(0,1) distr
  poke :: d -> Double -> Double
  

data UniDistr = UniDistr
              deriving (Show)

instance Distribution UniDistr where
  poke UniDistr = id

data ExpDistr = ExpDistr { lambda :: Double }
              deriving (Show)

instance Distribution ExpDistr where
  poke d x = let l = lambda d in (log (1 - x)) / (-l)

data Distr where
  MkDistr :: (Show d, Distribution d) => d -> Distr
deriving instance Show Distr

