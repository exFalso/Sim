module Main where

import Sim
import ChainDef
import Distribution
import Node
import EventResult

_LAMBDA = 0.00234797779287925
_MU     = 0.01690544010873064

chainDef = do
  src <- createSource (Logged "Customer")
  sink <- createSink (Logged "Server") (ExpDistr _MU)
  trans src sink (ExpDistr _LAMBDA)

_DEADLINE = 10000000

simulation = flushAt _DEADLINE

main = do
  results <- runSimT simulation chainDef
  
  let serverResults = reverse $ (map (\(EventResult _ _ t0 t1) -> (t0, t1)) . snd) (head results)
  print (length serverResults)
  let diffs = map (uncurry (flip (-))) serverResults
  let lol = map (\(a, (b, c)) -> (a, c - b)) . uncurry ($)
            . foldl (\(f, r) x -> let (g, r2) = insert 0 x r in (f . g, r2)) (id, []) $ serverResults
  let meanQueue = sum (map (\(a, b) -> fromIntegral a * b) lol) / _DEADLINE
  print meanQueue

type Queuey = (Integer, (Double, Double))

mean m = sum m / fromIntegral (length m)

insert :: Double -> (Double, Double) -> [Queuey] -> ([Queuey] -> [Queuey], [Queuey])
insert z (s, b) [] = (((0, (z, s)) :), [(1, (s, b))])
insert z (s, b) ((n, (s2, b2)) : rest) = if s > b2
                                       then let (f, rst) = insert b2 (s, b) rest in
                                         (((n, (s2, b2)) :) . f, rst)
                                       else
                                         (((n, (s2, s)) :), insertRest b b2 ((n, (s, b2)) : rest))
                                           where
                                             insertRest x l [] = [(1, (l, x))]
                                             insertRest x _ ((n, (s, b)) : rest) =
                                               if x < b then ((n + 1, (s, x)) : (n, (x, b)) : rest)
                                               else (n + 1, (s, b)) : insertRest x b rest
