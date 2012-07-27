module Term(Term(..)) where

import qualified Data.Text as Text

data Term
  = Word Text.Text
  | Integer Integer
  | Inexact Double
  | Quotation [Term]
  | Definition Term Term
