{-# LANGUAGE OverloadedStrings #-}

module Kitten.Vocabulary
  ( global
  , globalName
  , intrinsic
  , intrinsicName
  ) where

import Data.Text (Text)
import Kitten.Name (Qualifier(..))

global :: Qualifier
global = Qualifier [globalName]

globalName :: Text
globalName = ""

intrinsic :: Qualifier
intrinsic = Qualifier [globalName, intrinsicName]

intrinsicName :: Text
intrinsicName = "kitten"
