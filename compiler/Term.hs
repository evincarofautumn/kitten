module Term(Term(..)) where

import qualified Data.Text as Text

data Term
  = Word Text.Text
  | Integer Integer
  | Float Double
  | Quotation [Term]
  | Definition Term Term
