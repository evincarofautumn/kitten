{-# OPTIONS -cpp -pgmPcpphs -optP--cpp #-}
{-# LANGUAGE OverloadedStrings #-}
 
module Compile
  ( compile
  ) where

import Error
import Program
import Term

import Data.List
import Text ((+++))
import qualified Text as Text

data Context = Context
  { quotation :: Bool
  , definitions :: [Text.Text]
  }

quoted :: Context -> Context
quoted e = e { quotation = True }

defining :: Context -> Text.Text -> Context
e `defining` s = e { definitions = definitions e ++ [s] }

#include "builtins.h"
#define INIT(NAME) #NAME,
#define LAST(NAME) #NAME
builtins :: [Text.Text]
builtins = [ KITTEN_BUILTINS(INIT, LAST) ]
#undef LAST
#undef INIT

emptyContext :: Context
emptyContext = Context False []

compile :: Program -> ErrorMonad Text.Text
compile (Program terms) = do
  result <- compileWith emptyContext terms
  return $ Text.concat 
    [ "#include <kitten.h>\nKITTEN_PROGRAM("
    , Text.unwords result
    , ")"
    ]

compileWith :: Context -> [Term] -> ErrorMonad [Text.Text]
compileWith _ [] = Right []
compileWith here terms = case compileTerm here (head terms) of
  Right (first, next) -> case compileWith next (tail terms) of
    Right rest -> Right $ first : rest
    compileError -> compileError
  Left compileError -> Left compileError

compileTerm :: Context -> Term -> ErrorMonad (Text.Text, Context)
compileTerm here value =
  case value of
    Inexact f ->
      if quotation here
        then Right ("MKF(" +++ Text.show f +++ ")", here)
        else Right ("PUSHF(" +++ Text.show f +++ ")", here)
    Integer i ->
      if quotation here
        then Right ("MKI(" +++ Text.show i +++ ")", here)
        else Right ("PUSHI(" +++ Text.show i +++ ")", here)
    Quotation q ->
      if quotation here
        then case compileQuotation here q of
          Right result -> Right ("MKQ(" +++ result +++ ")", here)
          Left compileError -> Left compileError
        else case compileQuotation here q of
          Right result -> Right ("PUSHQ(" +++ result +++ ")", here)
          Left compileError -> Left compileError
    Word w ->
      if quotation here
        then case w `elemIndex` definitions here of
          Just n -> Right ("MKW(" +++ Text.show n +++ ")", here)
          Nothing -> if w `elem` builtins
            then Right ("word_new(WORD_" +++ w +++ ")", here)
            else Left . CompileError $ "Undefined word \"" +++ w +++ "\""
        else case w `elemIndex` definitions here of
          Just n -> Right ("DO(" +++ Text.show n +++ ")", here)
          Nothing -> if w `elem` builtins
            then Right ("BUILTIN(" +++ w +++ ")", here)
            else Left . CompileError $ "Undefined word \"" +++ w +++ "\""
    Definition (Word name) body@(Quotation _) ->
      if quotation here
        then Left . CompileError
          $ "A definition cannot appear inside a quotation."
        else case compiledBody of
          Right terms -> Right
            ("DEF(" +++ Text.unwords terms +++ ")", next)
          Left compileError -> Left compileError
          where
            compiledBody = compileWith (quoted next) [body]
            next = here `defining` name
    _ -> Left . CompileError $ "Unable to compile malformed term."

compileQuotation :: Context -> [Term] -> ErrorMonad Text.Text
compileQuotation here terms = case compiledBody of
  Right compiledTerms ->
    Right $ prefix +++ Text.intercalate ", " compiledTerms
  Left compileError -> Left compileError
  where
    compiledBody = compileWith (quoted here) terms
    prefix
      = if null terms
          then "0, 0"
          else Text.show (length terms) +++ ", "
