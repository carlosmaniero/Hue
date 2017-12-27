module Hue.Context
    ( HueContext(..) ) where

data HueContext =
  HueContext { hueContextId :: Int
             } deriving (Show, Eq)
