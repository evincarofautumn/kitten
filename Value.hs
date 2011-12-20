module Value(Value(..), compileValue) where

import Data.Char (toUpper)
import Data.List (intercalate)

data Value = Word String
           | Integer Integer
           | Float Double
           | Quotation [Value]

data Environment = Environment {
  inQuotation :: Bool,
  definitions :: [String]
}

compileValue :: Value -> String
compileValue = compileValue' defaultEnvironment
  where
    compileValue' environment value =
      case value of
        Float f ->
          if inQuotation environment
            then "MKF(" ++ show f ++ ")"
            else "PUSHF(" ++ show f ++ ")"
        Integer i ->
          if inQuotation environment
            then "MKI(" ++ show i ++ ")"
            else "PUSHI(" ++ show i ++ ")"
        Quotation q ->
          if inQuotation environment
            then "MKQ(" ++ showQuotationContents environment q ++ ")"
            else "PUSHQ(" ++ showQuotationContents environment q ++ ")"
        Word w ->
          if inQuotation environment
            then "W" ++ (map toUpper $ w)
            else map toUpper $ w
    defaultEnvironment = Environment False []
    showQuotationContents environment values = (show . length $ values) ++ ", "
      ++ (intercalate ", " . map (compileValue' environment
      { inQuotation = True }) $ values)
