{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Id
  ( DefId
  , DefIdGen
  , Id(..)
  , TypeId
  , TypeIdGen
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

type DefId = Id DefSpace
type TypeId = Id TypeSpace

instance Show (Id n) where
  show = T.unpack . toText

instance ToText (Id n) where
  toText (Id i) = "_" <> showText i

newtype IdGen (n :: Namespace) = IdGen (Id n)

type DefIdGen = IdGen DefSpace
type TypeIdGen = IdGen TypeSpace

genId :: IdGen n -> (Id n, IdGen n)
genId (IdGen i) = (i, IdGen (succ i))

mkIdGen :: IdGen n
mkIdGen = mkIdGenFrom (Id 0)

mkIdGenFrom :: Id n -> IdGen n
mkIdGenFrom = IdGen
