{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Kitten.KindedId where

import qualified Data.Text as T

import Kitten.Id
import Kitten.Kind
import Kitten.Util.Text (ToText(..))

newtype KindedId (a :: Kind) = KindedId { unkinded :: TypeId }
  deriving (Enum, Eq, Ord)

newtype KindedGen (a :: Kind) = KindedGen (KindedId a)

genKinded :: KindedGen a -> (KindedId a, KindedGen a)
genKinded (KindedGen i) = (i, KindedGen (succ i))

mkKindedGen :: KindedGen a
mkKindedGen = KindedGen (KindedId (Id 0))

instance Show (KindedId a) where
  show = T.unpack . toText

instance ToText (KindedId a) where
  toText = toText . unkinded
