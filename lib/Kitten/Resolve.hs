{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Resolve
  ( Resolved(..)
  , Value(..)
  , resolve
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List
import Data.Monoid

import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Error
import Kitten.Name
import Kitten.Fragment
import Kitten.Term (Term)

import qualified Kitten.Term as Term

data Resolved
  = Value Value
  | Builtin Builtin
  | Scoped Resolved
  | Local Name
  | Compose [Resolved]

instance Show Resolved where
  show resolved = case resolved of
    Value value -> show value
    Builtin builtin -> show builtin
    Scoped term -> concat ["enter ", show term, " leave"]
    Local (Name index) -> "local" ++ show index
    Compose terms -> unwords $ map show terms

data Value
  = Word Name
  | Int Int
  | Bool Bool
  | Text String
  | Vec [Value]
  | Tuple [Value]
  | Fun Resolved

instance Show Value where
  show v = case v of
    Word (Name name) -> '@' : show name
    Int value -> show value
    Bool value -> if value then "true" else "false"
    Text value -> show value
    Vec values -> "[" ++ showVector values ++ "]"
    Tuple values -> "(" ++ showVector values ++ ")"
    Fun term -> "{" ++ show term ++ "}"
    where
    showVector = unwords . map show . reverse

data Env = Env
  { envPrelude :: [Def Resolved]
  , envDefs :: [Def Term]
  , envScope :: [String]
  }

withLocal :: String -> Resolution a -> Resolution a
withLocal name action = do
  modify $ \ env@Env{..} -> env { envScope = name : envScope }
  result <- action
  modify $ \ env@Env{..} -> env { envScope = tail envScope }
  return result

type Resolution = StateT Env (Either CompileError)

resolve
  :: [Def Resolved]
  -> Fragment Term
  -> Either CompileError (Fragment Resolved)
resolve prelude (Fragment _annos defs term) = flip evalStateT env0
  $ Fragment _annos <$> resolveDefs defs <*> resolveTerm term
  where env0 = Env prelude defs []

resolveDefs :: [Def Term] -> Resolution [Def Resolved]
resolveDefs defs = (<>) <$> gets envPrelude <*> mapM resolveDef defs
  where resolveDef (Def name body) = Def name <$> resolveTerm body

resolveTerm :: Term -> Resolution Resolved
resolveTerm unresolved = case unresolved of
  Term.Value value -> resolveValue value
  Term.Builtin name -> return $ Builtin name
  Term.Compose terms -> Compose <$> mapM resolveTerm terms
  Term.Lambda name term -> withLocal name $ Scoped <$> resolveTerm term

fromValue :: Resolved -> Value
fromValue (Value value) = value
fromValue _ = error "Resolve.fromValue: not a value"

resolveValue :: Term.Value -> Resolution Resolved
resolveValue unresolved = case unresolved of
  Term.Word name -> do
    mLocalIndex <- gets $ localIndex name
    case mLocalIndex of
      Just index -> return . Local $ Name index
      Nothing -> do
        mDefIndex <- gets $ defIndex name
        case mDefIndex of
          Just index -> return . Value . Word $ Name index
          Nothing -> lift . Left . CompileError $ concat
            ["Unable to resolve word '", name, "'"]
  Term.Fun term -> Value . Fun <$> resolveTerm term
  Term.Vec terms -> Value . Vec <$> resolveVector terms
  Term.Tuple terms -> Value . Tuple <$> resolveVector terms
  Term.Int value -> return . Value $ Int value
  Term.Bool value -> return . Value $ Bool value
  Term.Text value -> return . Value $ Text value
  where resolveVector = mapM $ fmap fromValue . resolveValue

localIndex :: String -> Env -> Maybe Int
localIndex name = elemIndex name . envScope

defIndex :: String -> Env -> Maybe Int
defIndex expected Env{..} = findExpected envPrelude
  <|> ((+ length envPrelude) <$> findExpected envDefs)
  where findExpected = findIndex $ (== expected) . defName
