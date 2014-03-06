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
import Data.Set (Set)
import Data.Text (Text)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import Text.Parsec.Error

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.AST
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
import Kitten.Util.Monad

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
compile Compile.Config{..} nameGen
  = liftM (mapLeft sort) . runEitherT $ do
  parsed <- hoistEither $ parseSource firstLine name source
  substituted <- hoistEither
    =<< lift (substituteImports libraryDirectories parsed)
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

  fmap nub $ filterM doesFileExist
    =<< mapMaybeM canonicalImport searchDirectories

  where
  canonicalImport :: FilePath -> IO (Maybe FilePath)
  canonicalImport path = catchIOError
    (Just <$> canonicalizePath (path </> T.unpack importName <.> "ktn"))
    $ \e -> if isDoesNotExistError e then return Nothing else ioError e

substituteImports
  :: [FilePath]
  -> Fragment Term
  -> IO (Either [ErrorGroup] (Fragment Term))
substituteImports libraryDirectories fragment = runEitherT $ do
  substituted <- go
    (fragmentImports fragment)
    S.empty
    (V.toList (fragmentDefs fragment))
  return fragment { fragmentDefs = V.fromList substituted }
  where
  go
    :: [Import]
    -> Set Text
    -> [TermDef Term]
    -> EitherT [ErrorGroup] IO [TermDef Term]
  go [] _ defs = return defs
  go (current:remaining) seenNames defs = let
    name = importName current
    location = importLocation current
    in if name `S.member` seenNames
      then go remaining seenNames defs
      else do
        possible <- lift $ locateImport libraryDirectories name
        case possible of
          [filename] -> do
            source <- lift $ T.readFileUtf8 filename
            parsed <- hoistEither $ parseSource 1 filename source
            go
              (fragmentImports parsed ++ remaining)
              (S.insert name seenNames)
              (V.toList (fragmentDefs parsed) ++ defs)
          [] -> err location $ T.concat
            ["missing import '", name, "'"]
          _ -> err location $ T.concat
            ["ambiguous import '", name, "'"]
  err loc = left . (:[]) . oneError . CompileError loc Error
