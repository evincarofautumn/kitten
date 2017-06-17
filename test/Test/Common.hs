{-# LANGUAGE OverloadedStrings #-}

module Test.Common
  ( Sign(..)
  , ioPermission
  ) where

import Kitten.Name
import qualified Kitten.Vocabulary as Vocabulary

data Sign = Negative | Positive
  deriving (Eq, Ord, Show)

ioPermission :: [GeneralName]
ioPermission = [QualifiedName $ Qualified Vocabulary.global "IO"]
