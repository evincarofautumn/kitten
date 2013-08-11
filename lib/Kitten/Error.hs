module Kitten.Error
  ( CompileError(..)
  , parseError
  , printCompileErrors
  ) where

import Data.Ord (comparing)
import Text.Parsec.Error
import System.IO

import Kitten.Location

data CompileError
  = CompileError Location String
  | DuplicateError Location [Location] String
  | InternalError String
  | TypeError Location String
  | ErrorDetail Location String
  deriving (Eq)

instance Show CompileError where
  show compileError = case compileError of

    CompileError location message -> concat
      [ show location
      , ": compile error: "
      , message
      ]

    DuplicateError location locations name -> unlines
      $ concat
        [ show location
        , ": duplicate definition of "
        , name
        ]
      : map ((++ ": also defined here") . show) locations

    InternalError message -> "internal error: " ++ message

    TypeError location message -> concat
      [ show location
      , ": type error: "
      , message
      ]

    ErrorDetail location message -> concat
      [ show location
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
  message = showErrorMessages
    "or" "unknown" "expecting" "unexpected" "end of input"
    $ errorMessages err
  in CompileError location message

printCompileErrors :: [CompileError] -> IO ()
printCompileErrors errors
  = hPutStr stderr $ unlines (map show errors)
