{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -pgmPcpphs -optP--cpp #-}
 
module Compile
  ( compile
  ) where

import Error
import Program
import Term

import Data.List
import Text ((+++))
import qualified Text

data Context = Context
  { quotation   :: Bool
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

compileWith
  :: Context
  -> [Term]
  -> ErrorMonad [Text.Text]
compileWith _ [] = Right []
compileWith here terms = do
  (first, next) <- compileTerm here $ head terms
  rest <- compileWith next $ tail terms
  return $ first : rest

compileTerm
  :: Context
  -> Term
  -> ErrorMonad (Text.Text, Context)
compileTerm here value
  = case value of
      Inexact f   -> return (compileInexact here f, here)
      Integer i   -> return (compileInteger here i, here)
      Quotation q -> compileQuotation here q
      Word w      -> compileWord here w
      Definition (Word name) body@(Quotation _)
        -> compileDefinition here name body
      _ -> throwError $ CompileError "Unable to compile malformed term."

compileQuotation
  :: Context
  -> [Term]
  -> ErrorMonad (Text.Text, Context)
compileQuotation here terms
  = if quotation here then compileInside else compileOutside
  where
    compileInside = do
      result <- compileQuotation'
      return ("MKQ(" +++ result +++ ")", here)

    compileOutside = do
      result <- compileQuotation'
      return ("PUSHQ(" +++ result +++ ")", here)

    compiledBody = compileWith (quoted here) terms

    compileQuotation' = do
      compiledTerms <- compiledBody
      return $ prefix +++ Text.intercalate ", " compiledTerms

    prefix
      = if null terms
          then "0, 0"
          else Text.show (length terms) +++ ", "

compileInexact
  :: Context
  -> Double
  -> Text.Text
compileInexact here f
  = if quotation here
      then "MKF(" +++ Text.show f +++ ")"
      else "PUSHF(" +++ Text.show f +++ ")"

compileInteger
  :: Context
  -> Integer
  -> Text.Text
compileInteger here i
  = if quotation here
      then "MKI(" +++ Text.show i +++ ")"
      else "PUSHI(" +++ Text.show i +++ ")"

compileWord
  :: Context
  -> Text.Text
  -> ErrorMonad (Text.Text, Context)
compileWord here name
  = if quotation here then compileInside else compileOutside
  where
    compileInside
      = case name `elemIndex` definitions here of
        Just index ->
          return ("MKW(" +++ Text.show index +++ ")", here)
        Nothing ->
          if name `elem` builtins
            then return ("word_new(WORD_" +++ name +++ ")", here)
            else throwError . CompileError
              $ "Undefined word \"" +++ name +++ "\""

    compileOutside
      = case name `elemIndex` definitions here of
        Just index ->
          return ("DO(" +++ Text.show index +++ ")", here)
        Nothing ->
          if name `elem` builtins
            then return ("BUILTIN(" +++ name +++ ")", here)
            else throwError . CompileError
              $ "Undefined word \"" +++ name +++ "\""

compileDefinition
  :: Context
  -> Text.Text
  -> Term
  -> ErrorMonad (Text.Text, Context)
compileDefinition here name body
  = if quotation here
      then throwError
        $ CompileError "A definition cannot appear inside a quotation."
      else do
        terms <- compiledBody
        return ("DEF(" +++ Text.unwords terms +++ ")", next)
  where
    compiledBody = compileWith (quoted next) [body]
    next = here `defining` name
