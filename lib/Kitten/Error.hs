{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Error
  ( Label(..)
  , CompileError(..)
  , ErrorGroup(..)
  , oneError
  , parseError
  , printCompileErrors
  ) where

import Control.Applicative
import Control.Monad.Trans.State
import Data.Function
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Traversable (for)
import Text.Parsec.Error
import System.IO

import qualified Data.Text as T

import Kitten.Location
import Kitten.Util.Text (ToText(..))

data ErrorGroup = ErrorGroup [CompileError]
  deriving (Eq)

oneError :: CompileError -> ErrorGroup
oneError = ErrorGroup . (:[])

instance Ord ErrorGroup where
  compare (ErrorGroup (a:_)) (ErrorGroup (b:_)) = compare a b
  compare (ErrorGroup _) (ErrorGroup _) = EQ

instance ToText [ErrorGroup] where
  toText gs = T.intercalate "\n"
    $ flip evalState [] $ for gs $ \ (ErrorGroup g) -> do
      fmap (T.unlines . catMaybes) . for g $ \e -> do
        alreadySaid <- gets (e `elem`)
        if alreadySaid then return Nothing
          else do modify (e :); return $ Just (toText e)

data Label
  = Error
  | Note
  deriving (Eq, Ord)

instance ToText Label where
  toText Error = "error"
  toText Note = "note"

data CompileError = CompileError !Location !Label !Text
  deriving (Eq)

instance Show CompileError where
  show = T.unpack . toText

instance ToText CompileError where
  toText compileError = case compileError of
    CompileError location category message -> T.intercalate ": "
      [ toText location
      , toText category
      , message
      ]

instance Ord CompileError where
  compare (CompileError loc1 cat1 _) (CompileError loc2 cat2 _)
    = compare (cat1, loc1) (cat2, loc2)

parseError :: ParseError -> ErrorGroup
parseError err = ErrorGroup $ unexpecteds
  ++ if null expected
    then []
    else (:[]) . CompileError loc Note . T.pack $ unwords
      [ "expecting"
      , oxfordOr . nub . filter (not . null)
        $ map messageString expected
      ]
  where
  loc = Location
    { locationStart = errorPos err
    , locationIndent = 0
    }

  sysUnexpected, unexpected, expected :: [Message]
  (sysUnexpected, unexpected, expected)
    = flip evalState (errorMessages err) $ (,,)
      <$> state (span (SysUnExpect "" ==))
      <*> state (span (UnExpect "" ==))
      <*> state (span (Expect "" ==))

  unexpecteds :: [CompileError]
  unexpecteds = ((++) `on` unexpectedMessages) sysUnexpected unexpected

  unexpectedMessage :: Message -> CompileError
  unexpectedMessage message = let
    string = messageString message
    in CompileError loc Error . T.pack $ unwords
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

printCompileErrors :: [ErrorGroup] -> IO ()
printCompileErrors = hPutStr stderr . T.unpack . toText
