{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Compile
  ( compile
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import Text.Parsec.Error
import Text.Parsec.Pos

import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Error
import Kitten.Infer
import Kitten.IR
import Kitten.Location
import Kitten.Optimize
import Kitten.Parse
import Kitten.Resolve
import Kitten.Scope
import Kitten.Tokenize
import Kitten.Types
import Kitten.Util.Either
import Kitten.Util.Monad

import qualified Kitten.IdMap as Id
import qualified Kitten.Util.Text as T

liftParseError :: Either ParseError a -> Either [ErrorGroup] a
liftParseError = mapLeft ((:[]) . parseError)

parseSource
  :: Int
  -> String
  -> Text
  -> Either [ErrorGroup] (Fragment ParsedTerm)
parseSource line name source = do
  tokenized <- liftParseError $ tokenize line name source
  mapLeft (:[]) $ parse name tokenized

compile
  :: Config
  -> Program
  -> IO (Either [ErrorGroup] (Program, Int, Type Scalar))
compile config@Config{..} program
  = liftM (mapLeft sort) . runEitherT $ do
  parsed <- fmap
    (\fragment -> if configImplicitPrelude then fragment
      { fragmentImports = Import
        { importName = "Prelude"
        , importLocation = Location
          { locationStart = initialPos "Prelude"
          , locationIndent = 0
          }
        } : fragmentImports fragment
      } else fragment)
    $ hoistEither (parseSource configFirstLine configName configSource)

  (substituted) <- hoistEither
    =<< lift (substituteImports configLibraryDirectories parsed)

  -- Applicative rewriting must take place after imports have been
  -- substituted, so that all operator declarations are in scope.
  (postfix, program') <- hoistEither . mapLeft (:[]) $ rewriteInfix program substituted
  resolved <- hoistEither $ resolve postfix program'

  when configDumpResolved . lift $ hPrint stderr resolved

  let scoped = scope resolved
  when configDumpScoped . lift $ hPrint stderr scoped

  let (mTypedAndType, program'') = runK program' config $ typeFragment scoped
  (typed, type_) <- hoistEither mTypedAndType

  let (mErrors, program''') = ir typed program'' config
  void $ hoistEither mErrors

  let program'''' = optimize configOptimizations program'''

  return
    ( program''''
    , maybe 0 V.length $ Id.lookup entryId (programBlocks program)
    , type_
    )

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
  -> Fragment ParsedTerm
  -> IO (Either [ErrorGroup] (Fragment ParsedTerm))
substituteImports libraryDirectories fragment = runEitherT $ do
  (substitutedDefs, substitutedOperators, substitutedTypes) <- go
    (fragmentImports fragment)
    S.empty
    ( H.toList (fragmentDefs fragment)
    , fragmentOperators fragment
    , fragmentTypes fragment
    )
  return fragment
    { fragmentDefs = H.fromList substitutedDefs
    , fragmentOperators = substitutedOperators
    , fragmentTypes = substitutedTypes
    }
  where
  go
    :: [Import]
    -> Set Text
    -> ([(Text, Def ParsedTerm)], [Operator], HashMap Text TypeDef)
    -> EitherT [ErrorGroup] IO
      ([(Text, Def ParsedTerm)], [Operator], HashMap Text TypeDef)
  go [] _ acc = return acc
  go (currentModule : remainingModules) seenModules
    acc@(defs, operators, types) = let
    name = importName currentModule
    location = importLocation currentModule
    in if name `S.member` seenModules
      then go remainingModules seenModules acc
      else do
        possible <- lift $ locateImport libraryDirectories name
        case possible of
          [filename] -> do
            source <- lift $ T.readFileUtf8 filename
            parsed <- hoistEither $ parseSource 1 filename source
            go
              (fragmentImports parsed ++ remainingModules)
              (S.insert name seenModules)
              ( H.toList (fragmentDefs parsed) ++ defs
              , fragmentOperators parsed ++ operators
              , fragmentTypes parsed <> types
              )
          [] -> err location $ T.concat
            ["missing import '", name, "'"]
          _ -> err location $ T.concat
            ["ambiguous import '", name, "'"]
  err loc = left . (:[]) . oneError . CompileError loc Error
