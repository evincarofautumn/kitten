module Kitten
  ( Kitten.compile
  ) where

import Compile
import Error
import Parse

compile :: String -> String -> Error.Monad String
compile name source
  = case parse name source of
    Right parseResult -> Compile.compile parseResult
    Left parseError -> Left (ParseError parseError)
