{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Compile
  ( Compile.Config(..)
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

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Error
import Kitten.Fragment
import Kitten.Import
import Kitten.Infer
import Kitten.Name (NameGen)
import Kitten.Parse
import Kitten.Resolve
import Kitten.Scope
import Kitten.Term (Term)
import Kitten.Tokenize
import Kitten.Type
import Kitten.Typed (Typed)
import Kitten.Util.Either
import Kitten.Util.Function

import qualified Kitten.Compile.Config as Compile
import qualified Kitten.Util.Text as T

liftParseError :: Either ParseError a -> Either [ErrorGroup] a
liftParseError = mapLeft ((:[]) . parseError)

parseSource
  :: Int
  -> String
  -> Text
  -> Either [ErrorGroup] (Fragment Term)
parseSource line name source = do
  tokenized <- liftParseError $ tokenize line name source
  liftParseError $ parse name tokenized

compile
  :: Compile.Config
  -> NameGen
  -> IO (Either [ErrorGroup] (NameGen, Fragment Typed, Type Scalar))
compile Compile.Config{..} nameGen = liftM (mapLeft sort) . runEitherT $ do
  parsed <- hoistEither $ parseSource firstLine name source
  substituted <- hoistEither
    =<< lift (substituteImports libraryDirectories parsed [])
  resolved <- hoistEither $ resolve prelude substituted

  when dumpResolved . lift $ hPrint stderr resolved

  let scoped = scope resolved
  when dumpScoped . lift $ hPrint stderr scoped

  (nameGen', typed, type_) <- hoistEither
    $ typeFragment inferConfig stackTypes prelude scoped nameGen

  return (nameGen', typed, type_)

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
    $ \e -> if isDoesNotExistError e
      then return "" else ioError e

substituteImports
  :: [FilePath]
  -> Fragment Term
  -> [Import]
  -> IO (Either [ErrorGroup] (Fragment Term))
substituteImports libraryDirectories fragment inScope
  = runEitherT $ do

    let inScope' = V.toList (fragmentImports fragment) \\ inScope

    imports <- lift . forM inScope'
      $ \import_ -> do
        located <- locateImport libraryDirectories (importName import_)
        return (import_, located)

    imported <- forM imports $ \(import_, possible) -> case possible of
      [filename] -> do
        source <- lift $ T.readFileUtf8 filename
        parsed <- hoistEither $ parseSource 1 filename source
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
      $ (\defs -> mempty { fragmentDefs = defs }) . fragmentDefs
