{-# LANGUAGE RecordWildCards #-}

module Kitten.Operator
  ( Fixity(..)
  , FixityHint(..)
  , Operator(..)
  ) where

import Data.Text (Text)

import qualified Data.Text as T

import Kitten.Location

-- | The actual fixity of an operator.
data Fixity
  = Infix
  | InfixLeft
  | InfixRight
  | Postfix
  | Prefix
  deriving (Eq, Show)

-- | Whether a call is to a word that was originally postfix or infix.
data FixityHint = PostfixHint | InfixHint
  deriving (Eq, Show)

data Operator = Operator
  { operatorFixity :: !Fixity
  , operatorPrecedence :: !Precedence
  , operatorName :: !Text
  , operatorLocation :: !Location
  } deriving (Eq)

instance Show Operator where
  show (Operator fixity precedence name _) = unwords
    [ case fixity of
      Infix -> "infix"
      InfixLeft -> "infix_left"
      InfixRight -> "infix_right"
      Postfix -> "postfix"
      Prefix -> "prefix"
    , show precedence
    , T.unpack name
    ]

type Precedence = Int
