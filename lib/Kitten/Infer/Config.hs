module Kitten.Infer.Config
  ( Config(..)
  ) where

data Config = Config
  { enforceBottom :: !Bool
  , fragmentName :: String
  }
