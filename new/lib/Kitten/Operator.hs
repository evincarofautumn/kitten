{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Operator
  ( Associativity(..)
  , Fixity(..)
  , Operator(..)
  , Precedence(..)
  ) where

import Kitten.Name (Qualified)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as Pretty

data Operator = Operator
  { associativity :: !Associativity
  , name :: !Qualified
  , precedence :: !Precedence
  } deriving (Show)

data Fixity = Infix | Postfix
  deriving (Eq, Show)

data Associativity = Nonassociative | Leftward | Rightward
  deriving (Show)

newtype Precedence = Precedence Int
  deriving (Enum, Eq, Ord, Show, Pretty)

instance Bounded Precedence where
  minBound = Precedence 0
  maxBound = Precedence 9

instance Pretty Operator where
  pPrint operator = Pretty.hsep
    $ ("infix" :)
    $ (case associativity operator of
      Nonassociative -> id
      Leftward -> ("left" :)
      Rightward -> ("right" :))
    [ pPrint $ precedence operator
    , pPrint $ name operator
    ]
