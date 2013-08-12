{-# LANGUAGE OverloadedStrings #-}

module Kitten.Error
  ( CompileError(..)
  , parseError
  , printCompileErrors
  ) where

import Data.Monoid
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Vector (Vector)
import Text.Parsec.Error
import System.IO

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Location
import Kitten.Util.Text (ToText(..))

data CompileError
  = CompileError !Location !Text
  | DuplicateError !Location !(Vector Location) !Text
  | InternalError !Text
  | TypeError !Location !Text
  | ErrorDetail !Location !Text
  deriving (Eq)

instance Show CompileError where
  show = T.unpack . toText

instance ToText CompileError where
  toText compileError = case compileError of

    CompileError location message -> T.concat
      [ toText location
      , ": compile error: "
      , message
      ]

    DuplicateError location locations name -> T.unlines
      $ T.concat
        [ toText location
        , ": duplicate definition of "
        , name
        ]
      : V.toList
        (V.map ((<> ": also defined here") . toText) locations)

    InternalError message -> "internal error: " <> message

    TypeError location message -> T.concat
      [ toText location
      , ": type error: "
      , message
      ]

    ErrorDetail location message -> T.concat
      [ toText location
      , ": note: "
      , message
      ]

instance Ord CompileError where
  compare = comparing errorLocation

errorLocation :: CompileError -> Maybe Location
errorLocation compileError = case compileError of
  CompileError location _ -> Just location
  DuplicateError location _ _ -> Just location
  InternalError _ -> Nothing
  TypeError location _ -> Just location
  ErrorDetail location _ -> Just location

parseError :: ParseError -> CompileError
parseError err = let
  location = Location
    { locationStart = errorPos err
    , locationIndent = 0
    }
  message = T.pack . showErrorMessages
    "or" "unknown" "expecting" "unexpected" "end of input"
    $ errorMessages err
  in CompileError location message

printCompileErrors :: [CompileError] -> IO ()
printCompileErrors errors
  = hPutStr stderr $ unlines (map show errors)
