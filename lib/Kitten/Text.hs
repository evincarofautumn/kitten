{-|
Module      : Kitten.Text
Description : Text utility functions
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.Text
  ( capitalize
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

capitalize :: Text -> Text
capitalize x
  | Text.null x = x
  | otherwise
  = Text.toUpper (Text.singleton (Text.head x)) <> Text.tail x
