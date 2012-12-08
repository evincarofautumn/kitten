module Resolve
  ( Resolved(..)
  , resolveProgram
  ) where

import Control.Applicative
import Data.List

import Def
import Error
import Program
import Term (Term)

import qualified Term

data Resolved
  = Word String Int
  | Int Integer
  | Lambda String Resolved
  | Vec [Resolved]
  | Fun Resolved
  | Compose Resolved Resolved
  | Empty

instance Show Resolved where
  show (Word word _) = word
  show (Int value) = show value
  show (Lambda name body) = unwords ["\\", name, show body]
  show (Vec terms) = "(" ++ unwords (map show terms) ++ ")"
  show (Fun term) = "[" ++ show term ++ "]"
  show (Compose down top) = show down ++ ' ' : show top
  show Empty = ""

resolveProgram :: Program Term -> Either CompileError (Program Resolved)
resolveProgram (Program defs term)
  = Program <$> resolveDefs defs <*> resolveTerm defs term

resolveDefs :: [Def Term] -> Either CompileError [Def Resolved]
resolveDefs defs = mapM resolveDef defs
  where resolveDef (Def name body) = Def name <$> resolveTerm defs body

resolveTerm :: [Def Term] -> Term -> Either CompileError Resolved
resolveTerm defs unresolved = case unresolved of
  Term.Word name -> case defIndex defs name of
    Just index -> Right $ Word name index
    Nothing -> Left . CompileError $ concat
      ["Unable to resolve word '", name, "'"]
  Term.Fun term -> Fun <$> resolveTerm defs term
  Term.Vec terms -> Vec <$> resolveTerms terms
  Term.Compose down top
    -> Resolve.Compose <$> resolveTerm defs down <*> resolveTerm defs top
  Term.Int value -> Right $ Int value
  Term.Lambda name term -> Lambda name <$> resolveTerm defs term
  Term.Empty -> Right Empty
  where resolveTerms = mapM (resolveTerm defs)

defIndex :: [Def a] -> String -> Maybe Int
defIndex defs expected = findIndex ((== expected) . defName) defs
  where defName (Def name _) = name
