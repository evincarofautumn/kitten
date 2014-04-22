{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Id
  ( Id(..)
  , IdGen
  , genId
  , mkIdGen
  , mkIdGenFrom
  ) where

import Data.Monoid

import qualified Data.Text as T

import Kitten.Util.Text

newtype Id = Id Int
  deriving (Enum, Eq, Ord)

instance Show Id where
  show = T.unpack . toText

instance ToText Id where
  toText (Id i) = "_" <> showText i

newtype IdGen = IdGen Id

genId :: IdGen -> (Id, IdGen)
genId (IdGen i) = (i, IdGen (succ i))

mkIdGen :: IdGen
mkIdGen = mkIdGenFrom (Id 0)

mkIdGenFrom :: Id -> IdGen
mkIdGenFrom = IdGen
