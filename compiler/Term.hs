module Term(Term(..)) where

data Term
  = Word String
  | Integer Integer
  | Float Double
  | Quotation [Term]
  | Definition Term Term
