{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype IdMap a = IdMap (IntMap a)
  deriving (Monoid)

(!) :: IdMap a -> Id -> a
IdMap ids ! Id index = ids I.! index

adjust :: (a -> a) -> Id -> IdMap a -> IdMap a
adjust f (Id i) (IdMap ids) = IdMap $ I.adjust f i ids

empty :: IdMap a
empty = IdMap I.empty

fromList :: [(Id, a)] -> IdMap a
fromList = IdMap . foldr
  (\ (Id index, value) acc -> I.insert index value acc)
  I.empty

insert :: Id -> a -> IdMap a -> IdMap a
insert (Id index) value (IdMap ids) = IdMap $ I.insert index value ids

insertWith :: (a -> a -> a) -> Id -> a -> IdMap a -> IdMap a
insertWith f (Id index) value (IdMap ids)
  = IdMap $ I.insertWith f index value ids

lookup :: Id -> IdMap a -> Maybe a
lookup (Id index) (IdMap ids) = I.lookup index ids

map :: (a -> b) -> IdMap a -> IdMap b
map f (IdMap ids) = IdMap (I.map f ids)

member :: Id -> IdMap a -> Bool
member (Id index) (IdMap ids) = I.member index ids

notMember :: Id -> IdMap a -> Bool
notMember (Id index) (IdMap ids) = I.notMember index ids

singleton :: Id -> a -> IdMap a
singleton (Id i) x = IdMap $ I.singleton i x

toList :: IdMap a -> [(Id, a)]
toList (IdMap ids) = Prelude.map (first Id) (I.toList ids)
