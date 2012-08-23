module Kitten
  ( Kitten.compile
  ) where

import Compile
import Error
import Parse
import qualified Text

compile
  :: String
  -> String
  -> ErrorMonad Text.Text
compile name source
  = case parse name source of
    Right parseResult -> Compile.compile parseResult
    Left parseError   -> Left $ ParseError parseError
