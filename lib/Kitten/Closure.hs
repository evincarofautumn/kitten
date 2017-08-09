{-|
Module      : Kitten.Closure
Description : Converting closures to explicitly capture locals
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DataKinds #-}

module Kitten.Closure
  ( convertClosures
  ) where

import Kitten.Phase (Phase(..))
import Kitten.Term (Sweet)

convertClosures :: Sweet 'Postfix -> Sweet 'Scoped
convertClosures _ = error "TODO: convertClosures"
