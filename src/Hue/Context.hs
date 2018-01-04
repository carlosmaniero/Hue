module Hue.Context
    ( Context(..) ) where

newtype Context =
  Context { contextId :: Int
          } deriving (Show, Eq)
