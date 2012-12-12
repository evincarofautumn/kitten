{-# LANGUAGE RecordWildCards #-}

module Resolve
  ( Resolved(..)
  , resolveProgram
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List

import Builtin (Builtin)
import Def
import Error
import Name
import Program
import Term (Term)

import qualified Term

data Resolved
  = Word !Name
  | Builtin !Builtin
  | Int !Integer
  | Bool !Bool
  | Scoped !Resolved
  | Local !Name
  | Vec ![Resolved]
  | Fun !Resolved
  | Compose !Resolved !Resolved
  | Empty

instance Show Resolved where
  show (Word index) = show index
  show (Builtin name) = show name
  show (Int value) = show value
  show (Bool value) = if value then "true" else "false"
  show (Scoped term) = "scoped { " ++ show term ++ " }"
  show (Local index) = "local#" ++ show index
  show (Vec terms) = "(" ++ unwords (map show terms) ++ ")"
  show (Fun term) = "[" ++ show term ++ "]"
  show (Compose down top) = show down ++ ' ' : show top
  show Empty = ""

data Env = Env
  { envDefs :: [Def Term]
  , envScope :: [String]
  }

enter :: String -> Env -> Env
enter name env = env { envScope = envScope env ++ [name] }

leave :: Env -> Env
leave env = env { envScope = init $ envScope env }

type Resolution = StateT Env (Either CompileError)

resolveProgram :: Program Term -> Either CompileError (Program Resolved)
resolveProgram (Program defs term)
  = evalStateT (Program <$> resolveDefs defs <*> resolveTerm term) env0
  where env0 = Env defs []

resolveDefs :: [Def Term] -> Resolution [Def Resolved]
resolveDefs defs = mapM resolveDef defs
  where resolveDef (Def name body) = Def name <$> resolveTerm body

resolveTerm :: Term -> Resolution Resolved
resolveTerm unresolved = case unresolved of
  Term.Word name -> do
    mLocalIndex <- gets $ localIndex name
    case mLocalIndex of
      Just index -> return . Local $ Name index
      Nothing -> do
        mDefIndex <- gets $ defIndex name
        case mDefIndex of
          Just index -> return . Word $ Name index
          Nothing -> lift . Left . CompileError $ concat
            ["Unable to resolve word '", name, "'"]
  Term.Builtin name -> return $ Builtin name
  Term.Fun term -> Fun <$> resolveTerm term
  Term.Vec terms -> Vec <$> mapM resolveTerm terms
  Term.Compose down top
    -> Resolve.Compose <$> resolveTerm down <*> resolveTerm top
  Term.Int value -> return $ Int value
  Term.Bool value -> return $ Bool value
  Term.Lambda name term -> do
    modify $ enter name
    resolved <- resolveTerm term
    modify leave
    return $ Scoped resolved
  Term.Empty -> return Empty

localIndex :: String -> Env -> Maybe Int
localIndex name = elemIndex name . envScope

defIndex :: String -> Env -> Maybe Int
defIndex expected = findIndex ((== expected) . defName) . envDefs
  where defName (Def name _) = name
