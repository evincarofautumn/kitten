{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Kind
  ( Kind(..)
  , KindProxy(..)
  , ReifyKind(..)
  ) where

import Kitten.Util.Text

data Kind = Row | Scalar

data KindProxy (a :: Kind) = KindProxy

class ReifyKind (a :: Kind) where
  reifyKind :: KindProxy a -> Kind

instance ReifyKind Row where
  reifyKind _ = Row

instance ReifyKind Scalar where
  reifyKind _ = Scalar

instance ToText Kind where
  toText Row = "row"
  toText Scalar = "scalar"
