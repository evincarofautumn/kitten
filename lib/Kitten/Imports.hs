module Kitten.Imports
  ( locateImport
  , substituteImports
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.IO.Error

import Kitten.Error
import Kitten.Fragment
import Kitten.Term

locateImport
  :: String
  -> [FilePath]
  -> IO [FilePath]
locateImport importName libraryDirectories = do
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
    (canonicalizePath $ path </> importName <.> "ktn")
    $ \ e -> if isDoesNotExistError e
      then return "" else ioError e

substituteImports
  :: Fragment Value Term
  -> Either [CompileError] (Fragment Value Term)
substituteImports fragment = return fragment
