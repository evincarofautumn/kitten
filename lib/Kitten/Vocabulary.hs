{-|
Module      : Kitten.Vocabulary
Description : Namespaces
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

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
