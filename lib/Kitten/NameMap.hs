module Kitten.NameMap
  ( NameMap
  , (!)
  , empty
  , fromList
  , insert
  , lookup
  , member
  ) where

import Data.IntMap (IntMap)
import Prelude hiding (lookup)

import qualified Data.IntMap as I

import Kitten.Name

newtype NameMap a = NameMap (IntMap a)

(!) :: NameMap a -> Name -> a
NameMap names ! Name index = names I.! index

empty :: NameMap a
empty = NameMap I.empty

fromList :: [(Name, a)] -> NameMap a
fromList = NameMap . foldr
  (\ (Name index, value) acc -> I.insert index value acc)
  I.empty

insert :: Name -> a -> NameMap a -> NameMap a
insert (Name index) value (NameMap names)
  = NameMap $ I.insert index value names

lookup :: Name -> NameMap a -> Maybe a
lookup (Name index) (NameMap names)
  = I.lookup index names

member :: Name -> NameMap a -> Bool
member (Name index) (NameMap names)
  = I.member index names
