{-|
Module      : Kitten.IO
Description : I/O utilities
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

module Kitten.IO
  ( readFileUtf8
  ) where

import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text

-- | Read a UTF-8-encoded source file and decode it to UTF-16 'Text'.

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 = fmap Text.decodeUtf8 . ByteString.readFile
