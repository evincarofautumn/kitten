{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Kitten.IdMap
  ( IdMap(..)
  , (!)
  , adjust
  , empty
  , fromList
  , insert
  , insertWith
  , lookup
  , map
  , member
  , notMember
  , singleton
  , toList
  ) where

import Control.Arrow
import Data.IntMap (IntMap)
import Data.Monoid
import Prelude hiding (lookup, map)

import qualified Data.IntMap as I
import qualified Prelude

import Kitten.Id

newtype IdMap (n :: Namespace) a = IdMap (IntMap a)
  deriving (Monoid)

(!) :: IdMap n a -> Id n -> a
IdMap ids ! Id index = ids I.! index

adjust :: (a -> a) -> Id n -> IdMap n a -> IdMap n a
adjust f (Id i) (IdMap ids) = IdMap $ I.adjust f i ids

empty :: IdMap n a
empty = IdMap I.empty

fromList :: [(Id n, a)] -> IdMap n a
fromList = IdMap . foldr
  (\ (Id index, value) acc -> I.insert index value acc)
  I.empty

insert :: Id n -> a -> IdMap n a -> IdMap n a
insert (Id index) value (IdMap ids) = IdMap $ I.insert index value ids

insertWith :: (a -> a -> a) -> Id n -> a -> IdMap n a -> IdMap n a
insertWith f (Id index) value (IdMap ids)
  = IdMap $ I.insertWith f index value ids

lookup :: Id n -> IdMap n a -> Maybe a
lookup (Id index) (IdMap ids) = I.lookup index ids

map :: (a -> b) -> IdMap n a -> IdMap n b
map f (IdMap ids) = IdMap (I.map f ids)

member :: Id n -> IdMap n a -> Bool
member (Id index) (IdMap ids) = I.member index ids

notMember :: Id n -> IdMap n a -> Bool
notMember (Id index) (IdMap ids) = I.notMember index ids

singleton :: Id n -> a -> IdMap n a
singleton (Id i) x = IdMap $ I.singleton i x

toList :: IdMap n a -> [(Id n, a)]
toList (IdMap ids) = Prelude.map (first Id) (I.toList ids)
