{-|
Module      : Kitten.Flatten
Description : Lifting quotations into top-level definitions
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}

module Kitten.Flatten
  ( flatten
  ) where

import Kitten.Dictionary (Dictionary)
import Kitten.Monad (K)
import Kitten.Name (Qualifier)
import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet)

flatten
  :: Dictionary
  -> Qualifier
  -> Sweet 'Typed
  -> K (Sweet 'Typed, Dictionary)
flatten _ _ _ = error "TODO: flatten terms"
