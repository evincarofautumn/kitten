{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Kind where

import Kitten.Util.Text (ToText(..))

data Kind = Scalar | Stack
  deriving (Eq)

-- | A helper data type for reification of a kind type,
-- better demonstrated than explained:
--
-- > toText (KindProxy :: KindProxy a)
--
data KindProxy (a :: Kind) = KindProxy

class ReifyKind (a :: Kind) where
  reifyKind :: KindProxy a -> Kind

instance ReifyKind 'Stack where
  reifyKind _ = Stack

instance ReifyKind 'Scalar where
  reifyKind _ = Scalar

instance ToText Kind where
  toText Stack = "stack"
  toText Scalar = "scalar"
