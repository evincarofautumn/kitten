{-# LANGUAGE TemplateHaskell #-}

module Kitten.Prelude
  ( prelude
  ) where

import Data.Monoid
import Data.Vector (Vector)

import Kitten.Compile
import Kitten.Def
import Kitten.Embed
import Kitten.Fragment
import Kitten.Resolve
import Kitten.Util

prelude :: Vector (Def Resolved)
prelude = fragmentDefs . fromRight
  $ compile [] mempty mempty "PRELUDE" preludeSource

preludeSource :: String
preludeSource = $(embed "prelude.ktn")
