{-# LANGUAGE OverloadedStrings #-}

module Kitten.Operator where

import Data.Text (Text)

import qualified Data.Text as T

import Kitten.Util.Text (ToText(..))

-- | The associativity of an infix operator.
data Associativity
  = NonAssociative
  | LeftAssociative
  | RightAssociative
  deriving (Eq, Show)

-- | Whether a call is to a word that was originally postfix or infix.
data Fixity = Infix | Postfix
  deriving (Eq)

instance ToText Fixity where
  toText Infix = "infix"
  toText Postfix = "postfix"

instance Show Fixity where
  show = T.unpack . toText

data Operator = Operator
  { operatorAssociativity :: !Associativity
  , operatorPrecedence :: !Precedence
  , operatorName :: !Text
  } deriving (Eq)

instance Show Operator where
  show (Operator fixity precedence name) = unwords
    [ case fixity of
      NonAssociative -> "infix"
      LeftAssociative -> "infix_left"
      RightAssociative -> "infix_right"
    , show precedence
    , T.unpack name
    ]

type Precedence = Int
