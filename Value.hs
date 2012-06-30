module Value(Value(..), compile) where

import CompileError

import Data.Char (toUpper)
import Data.List (elemIndex, intercalate)

data Value
  = Word String
  | Integer Integer
  | Float Double
  | Quotation [Value]
  | Definition Value Value

data Context = Context
  { quotation :: Bool
  , definitions :: [String]
  }

quoted :: Context -> Context
quoted e = e { quotation = True }

defining :: Context -> String -> Context
e `defining` s = e { definitions = definitions e ++ [s] }

builtins :: [String]
builtins =
  ["dup",
  "swap",
  "pop",
  "quote",
  "compose",
  "apply",
  "add",
  "sub",
  "mul",
  "div",
  "mod",
  "isf",
  "isi",
  "isq",
  "isw",
  "eq",
  "ne",
  "lt",
  "ge",
  "gt",
  "le",
  "if",
  "write",
  "putc",
  "trace"]

compile :: [Value] -> Either CompileError [String]
compile = compileWith emptyContext
  where
    emptyContext = Context False []
    compileWith :: Context -> [Value] -> Either CompileError [String]
    compileWith _ [] = Right []
    compileWith here values = case compileValue here (head values) of
      Right (first, next) -> case compileWith next (tail values) of
        Right rest -> Right $ first : rest
        compileError -> compileError
      Left compileError -> Left compileError
    compileValue :: Context -> Value -> Either CompileError (String, Context)
    compileValue here value =
      case value of
        Float f ->
          if quotation here
            then Right ("MKF(" ++ show f ++ ")", here)
            else Right ("PUSHF(" ++ show f ++ ")", here)
        Integer i ->
          if quotation here
            then Right ("MKI(" ++ show i ++ ")", here)
            else Right ("PUSHI(" ++ show i ++ ")", here)
        Quotation q ->
          if quotation here
            then case compileQuotation here q of
              Right result -> Right ("MKQ(" ++ result ++ ")", here)
              Left compileError -> Left compileError
            else case compileQuotation here q of
              Right result -> Right ("PUSHQ(" ++ result ++ ")", here)
              Left compileError -> Left compileError
        Word w ->
          if quotation here
            then case w `elemIndex` definitions here of
              Just n -> Right ("MKW(" ++ show n ++ ")", here)
              Nothing -> if w `elem` builtins
                then Right ('W' : map toUpper w, here)
                else Left . CompileError $ "Undefined word \"" ++ w ++ "\""
            else case w `elemIndex` definitions here of
              Just n -> Right ("DO(" ++ show n ++ ")", here)
              Nothing -> if w `elem` builtins
                then Right (map toUpper w, here)
                else Left . CompileError $ "Undefined word \"" ++ w ++ "\""
        Definition (Word name) body@(Quotation _) ->
          if quotation here
            then Left . CompileError $
              "A definition cannot appear inside a quotation."
            else case compiledBody of
              Right values -> Right
                (("DEF(" ++) . (++ ")") $ unwords values, next)
              Left compileError -> Left compileError
              where
                compiledBody = compileWith (quoted next) [body]
                next = here `defining` name
        _ -> Left . CompileError $ "Unable to compile malformed value."
    compileQuotation :: Context -> [Value] -> Either CompileError String
    compileQuotation here values = case compiledBody of
      Right compiledValues ->
        Right $ (if null values then "0, 0" else show (length values) ++ ", ")
          ++ intercalate ", " compiledValues
      Left compileError -> Left compileError
      where
        compiledBody = compileWith (quoted here) values
