module EventResult where

import Node

data EventResult = EventResult { eventFrom :: NodeId
                               , eventTo :: NodeId
                               , timeFrom :: Double
                               , timeTo :: Double }
                   deriving (Show)
