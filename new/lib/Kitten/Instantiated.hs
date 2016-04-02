{-# LANGUAGE OverloadedStrings #-}

module Kitten.Instantiated
  ( Instantiated(..)
  ) where

import Data.Hashable (Hashable(..))
import Kitten.Name (Qualified)
import Kitten.Type (Type)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Pretty as Pretty
import qualified Text.PrettyPrint as Pretty

data Instantiated = Instantiated
  { name :: !Qualified
  , types :: [Type]
  } deriving (Eq, Show)

instance Hashable Instantiated where
  hashWithSalt s (Instantiated n ts)
    = hashWithSalt s (0 :: Int, n, ts)

instance Pretty Instantiated where
  pPrint (Instantiated n ts) = Pretty.hcat
    [pPrint n, "::<", Pretty.list $ map pPrint ts, ">"]
