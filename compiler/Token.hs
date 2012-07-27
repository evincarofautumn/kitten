module Token where

import qualified Text as Text
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

data Located = Located SourcePos Token
  deriving (Eq)

instance Show Located where
  show (Located _ token) = show token
