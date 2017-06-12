{-|
Module      : Kitten.Operator
Description : Infix operator metadata
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

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

-- | Operator metadata for infix desugaring.

data Operator = Operator
  { associativity :: !Associativity
  , name :: !Qualified
  , precedence :: !Precedence
  } deriving (Show)

-- | Whether a word was declared infix (@+@) or postfix (@plus@).

data Fixity = Infix | Postfix
  deriving (Eq, Show)

-- | Whether an operator associates leftward:
--
-- > a + b + c = (a + b) + c
--
-- Rightward:
--
-- > a + b + c = a + (b + c)
--
-- Or not at all:
--
-- > a + b + c  // error

data Associativity = Nonassociative | Leftward | Rightward
  deriving (Show)

-- | The precedence level (from 0 to 9) of an operator; higher-precedence
-- operators bind more tightly than lower-precedence operators.

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
