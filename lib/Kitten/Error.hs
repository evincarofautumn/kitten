{-# LANGUAGE OverloadedStrings #-}

module Kitten.Error
  ( CompileError(..)
  , parseError
  , printCompileErrors
  ) where

import Control.Applicative
import Control.Monad.Trans.State
import Data.List
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

parseError :: ParseError -> [CompileError]
parseError err = concat
  [ unexpectedMessages sysUnexpected
  , unexpectedMessages unexpected
  , if null expected
    then []
    else (:[]) . ErrorDetail loc . T.pack $ unwords
      [ "expecting"
      , oxfordOr . nub . filter (not . null)
        $ map messageString expected
      ]
  ]
  where
  loc = Location
    { locationStart = errorPos err
    , locationIndent = 0
    }

  sysUnexpected, unexpected, expected :: [Message]
  (sysUnexpected, unexpected, expected)
    = flip evalState (errorMessages err) $ (,,)
      <$> state (span ((SysUnExpect "") ==))
      <*> state (span ((UnExpect "") ==))
      <*> state (span ((Expect "") ==))

  unexpectedMessage :: Message -> CompileError
  unexpectedMessage message = let
    string = messageString message
    in CompileError loc . T.pack $ unwords
      [ "unexpected"
      , if null string then "end of input" else string
      ]

  unexpectedMessages :: [Message] -> [CompileError]
  unexpectedMessages = nub . map unexpectedMessage

oxfordOr :: [String] -> String
oxfordOr [] = ""
oxfordOr [x] = x
oxfordOr [x, y] = unwords [x, "or", y]
oxfordOr [x, y, z] = concat [x, ", ", y, ", or ", z]
oxfordOr (x:xs) = concat [x, ", ", oxfordOr xs]

printCompileErrors :: [CompileError] -> IO ()
printCompileErrors errors
  = hPutStr stderr $ unlines (map show errors)
