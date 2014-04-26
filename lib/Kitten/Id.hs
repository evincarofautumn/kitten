{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Id
  ( Id(..)
  , Namespace(..)
  , IdGen
  , genId
  , mkIdGen
  , mkIdGenFrom
  ) where

import Data.Monoid

import qualified Data.Text as T

import Kitten.Util.Text

data Namespace
  = DefSpace
  | TypeSpace

newtype Id (n :: Namespace) = Id Int
  deriving (Enum, Eq, Ord)

instance Show (Id n) where
  show = T.unpack . toText

instance ToText (Id n) where
  toText (Id i) = "_" <> showText i

newtype IdGen (n :: Namespace) = IdGen (Id n)

genId :: IdGen n -> (Id n, IdGen n)
genId (IdGen i) = (i, IdGen (succ i))

mkIdGen :: IdGen n
mkIdGen = mkIdGenFrom (Id 0)

mkIdGenFrom :: Id n -> IdGen n
mkIdGenFrom = IdGen
