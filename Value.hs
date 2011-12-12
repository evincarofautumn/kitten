module Value(Value(..)) where

import Control.Arrow ((>>>))
import Data.List (intercalate)

data Value = Word String
           | Integer Integer
           | Float Double
           | Quotation [Value]

instance Show Value where
  show (Word w) = w
  show (Integer i) = show i
  show (Float f) = show f
  show (Quotation q) =
    map show >>> intercalate " " >>> ("[" ++) >>> (++ "]") $ q
