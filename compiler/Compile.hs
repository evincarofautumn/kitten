{-# OPTIONS -cpp -pgmPcpphs -optP--cpp #-}
 
module Compile
  ( compile
  ) where

import Error
import Program
import Term

import Data.List

data Context = Context
  { quotation :: Bool
  , definitions :: [String]
  }

quoted :: Context -> Context
quoted e = e { quotation = True }

defining :: Context -> String -> Context
e `defining` s = e { definitions = definitions e ++ [s] }

#include "builtins.h"
#define INIT(NAME) #NAME,
#define LAST(NAME) #NAME
builtins :: [String]
builtins = [ KITTEN_BUILTINS(INIT, LAST) ]
#undef LAST
#undef INIT

emptyContext :: Context
emptyContext = Context False []

compile :: Program -> Error.Monad String
compile (Program terms) = do
  result <- compileWith emptyContext terms
  return $ "#include <kitten.h>\nKITTEN_PROGRAM(" ++ unwords result ++ ")"

compileWith :: Context -> [Term] -> Error.Monad [String]
compileWith _ [] = Right []
compileWith here terms = case compileTerm here (head terms) of
  Right (first, next) -> case compileWith next (tail terms) of
    Right rest -> Right $ first : rest
    compileError -> compileError
  Left compileError -> Left compileError

compileTerm :: Context -> Term -> Error.Monad (String, Context)
compileTerm here value =
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
            then Right ("word_new(WORD_" ++ w ++ ")", here)
            else Left . CompileError $ "Undefined word \"" ++ w ++ "\""
        else case w `elemIndex` definitions here of
          Just n -> Right ("DO(" ++ show n ++ ")", here)
          Nothing -> if w `elem` builtins
            then Right ("BUILTIN(" ++ w ++ ")", here)
            else Left . CompileError $ "Undefined word \"" ++ w ++ "\""
    Definition (Word name) body@(Quotation _) ->
      if quotation here
        then Left . CompileError
          $ "A definition cannot appear inside a quotation."
        else case compiledBody of
          Right terms -> Right
            (("DEF(" ++) . (++ ")") $ unwords terms, next)
          Left compileError -> Left compileError
          where
            compiledBody = compileWith (quoted next) [body]
            next = here `defining` name
    _ -> Left . CompileError $ "Unable to compile malformed term."

compileQuotation :: Context -> [Term] -> Error.Monad String
compileQuotation here terms = case compiledBody of
  Right compiledTerms ->
    Right $ (if null terms then "0, 0" else show (length terms) ++ ", ")
      ++ intercalate ", " compiledTerms
  Left compileError -> Left compileError
  where
    compiledBody = compileWith (quoted here) terms
