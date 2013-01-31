{-# LANGUAGE TemplateHaskell #-}

module Kitten.Prelude
  ( prelude
  ) where

import Data.Vector (Vector)

import qualified Data.Vector as Vector

import Kitten.Compile
import Kitten.Def
import Kitten.Embed
import Kitten.Fragment
import Kitten.Resolve
import Kitten.Util

prelude :: Vector (Def Resolved)
prelude = fragmentDefs . fromRight
  $ compile [] Vector.empty "PRELUDE" preludeSource

preludeSource :: String
preludeSource = $(embed "prelude.ktn")
