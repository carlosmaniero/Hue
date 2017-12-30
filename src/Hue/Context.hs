module Hue.Context
    ( HueContext(..) ) where

newtype HueContext =
  HueContext { hueContextId :: Int
             } deriving (Show, Eq)
