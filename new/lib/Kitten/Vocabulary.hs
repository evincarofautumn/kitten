{-# LANGUAGE OverloadedStrings #-}

module Kitten.Vocabulary
  ( global
  , intrinsic
  , intrinsicName
  ) where

import Data.Text (Text)
import Kitten.Name (Qualifier(..), Root(..))

global :: Qualifier
global = Qualifier Absolute []

intrinsic :: Qualifier
intrinsic = Qualifier Absolute [intrinsicName]

intrinsicName :: Text
intrinsicName = "kitten"
