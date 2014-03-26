{-# LANGUAGE RecordWildCards #-}

module Kitten.Operator
  ( Associativity(..)
  , Fixity(..)
  , Operator(..)
  ) where

import Data.Text (Text)

import qualified Data.Text as T

import Kitten.Location

-- | The associativity of an infix operator.
data Associativity
  = NonAssociative
  | LeftAssociative
  | RightAssociative
  deriving (Eq, Show)

-- | Whether a call is to a word that was originally postfix or infix.
data Fixity = Postfix | Infix
  deriving (Eq, Show)

data Operator = Operator
  { operatorAssociativity :: !Associativity
  , operatorPrecedence :: !Precedence
  , operatorName :: !Text
  , operatorLocation :: !Location
  } deriving (Eq)

instance Show Operator where
  show (Operator fixity precedence name _) = unwords
    [ case fixity of
      NonAssociative -> "infix"
      LeftAssociative -> "infix_left"
      RightAssociative -> "infix_right"
    , show precedence
    , T.unpack name
    ]

type Precedence = Int
