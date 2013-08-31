{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Compile
  ( Config(..)
  , compile
  , locateImport
  , substituteImports
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.List
import Data.Monoid
import Data.Text (Text)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import Text.Parsec.Error

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Kitten.Error
import Kitten.Fragment
import Kitten.Import
import Kitten.Infer
import Kitten.Parse
import Kitten.Resolve
import Kitten.Resolved (Resolved, Value)
import Kitten.Scope
import Kitten.Term (Term)
import Kitten.Tokenize
import Kitten.Util.Either
import Kitten.Util.Function

import qualified Kitten.Term as Term

data Config = Config
  { dumpResolved :: !Bool
  , dumpScoped :: !Bool
  , libraryDirectories :: [FilePath]
  , name :: String
  , prelude :: !(Fragment Value Resolved)
  , source :: !Text
  , stack :: [Value]
  }

liftParseError :: Either ParseError a -> Either [CompileError] a
liftParseError = mapLeft ((:[]) . parseError)

parseSource
  :: String
  -> Text
  -> Either [CompileError] (Fragment Term.Value Term)
parseSource name source = do
  tokenized <- liftParseError $ tokenize name source
  liftParseError $ parse name tokenized

compile
  :: Config
  -> IO (Either [CompileError] (Fragment Value Resolved))
compile Config{..} = liftM (mapLeft sort) . runEitherT $ do
  parsed <- hoistEither $ parseSource name source
  substituted <- hoistEither
    =<< lift (substituteImports libraryDirectories parsed [])
  resolved <- hoistEither $ resolve prelude substituted

  when dumpResolved . lift $ hPrint stderr resolved
  hoistEither $ typeFragment stack prelude resolved

  let scoped = scope resolved
  when dumpScoped . lift $ hPrint stderr scoped

  return scoped

locateImport
  :: [FilePath]
  -> Text
  -> IO [FilePath]
locateImport libraryDirectories importName = do
  currentDirectory <- getCurrentDirectory

  let
    searchDirectories :: [FilePath]
    searchDirectories
      = "."
      : ("." </> "lib")
      : libraryDirectories
      ++ [currentDirectory, currentDirectory </> "lib"]

  nub <$> (filterM doesFileExist
    =<< mapM canonicalImport searchDirectories)

  where
  canonicalImport :: FilePath -> IO FilePath
  canonicalImport path = catchIOError
    (canonicalizePath $ path </> T.unpack importName <.> "ktn")
    $ \ e -> if isDoesNotExistError e
      then return "" else ioError e

substituteImports
  :: [FilePath]
  -> Fragment Term.Value Term
  -> [Import]
  -> IO (Either [CompileError] (Fragment Term.Value Term))
substituteImports libraryDirectories fragment inScope
  = runEitherT $ do

    let inScope' = V.toList (fragmentImports fragment) \\ inScope

    imports <- lift . forM inScope'
      $ \ import_ -> do
        located <- locateImport libraryDirectories (importName import_)
        return (import_, located)

    imported <- forM imports $ \ (import_, possible) -> case possible of
      [filename] -> do
        rawSource <- lift $ B.readFile filename
        let source = T.decodeUtf8 rawSource
        parsed <- hoistEither $ parseSource filename source
        hoistEither =<< lift
          (substituteImports libraryDirectories parsed inScope')

      -- FIXME fail with "hoistEither . Left . CompileError" or something
      -- FIXME better error messages
      [] -> (fail . T.unpack . T.concat)
        [ "unable to find import '"
        , importName import_
        , "'"
        ]
      _ -> (fail . T.unpack . T.concat)
        [ "ambiguous import '"
        , importName import_
        , "'"
        ]

    return $ foldr (<>) fragment
      $ for imported
      $ (\ defs -> mempty { fragmentDefs = defs }) . fragmentDefs
