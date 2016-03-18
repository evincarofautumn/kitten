module Kitten.IO
  ( readFileUtf8
  ) where

import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 = fmap Text.decodeUtf8 . ByteString.readFile
