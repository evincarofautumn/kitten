module Token where

import qualified Text

import Text.Parsec.Pos

data Token
  = QuotationOpen
  | QuotationClose
  | Definition
  | Layout
  | Text Text.Text  
  | Word Text.Text
  | Symbol Text.Text
  | Integer Integer
  | Rational Integer Integer
  | Inexact Double
  deriving (Eq, Show)

data Located = Located SourcePos Column Token
  deriving (Eq)

instance Show Located where
  show (Located _ _ token) = show token
