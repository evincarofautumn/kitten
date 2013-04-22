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
import Kitten.Location
import Kitten.Name
import Kitten.Fragment
import Kitten.Term (Term)
import Kitten.Util.List

import qualified Kitten.Term as Term

data Resolved
  = Push Value Location
  | Builtin Builtin Location
  | Scoped [Resolved]
  | Local Name Location
  | Closed Name Location
  | Compose [Resolved]
  deriving (Eq)

instance Show Resolved where
  show resolved = case resolved of
    Push value _ -> show value
    Builtin builtin _ -> show builtin
    Scoped terms -> unwords $ "\\" : map show terms
    Local (Name index) _ -> "local" ++ show index
    Closed (Name index) _ -> "closed" ++ show index
    Compose terms -> unwords $ map show terms

data Value
  = Word Name
  | Int Int
  | Bool Bool
  | Text String
  | Vec [Value]
  | Tuple [Value]
  | Fun [Resolved]
  | Closure [Name] [Resolved]
  | Closure' [Value] [Resolved]
  deriving (Eq)

instance Show Value where
  show v = case v of
    Word (Name name) -> '@' : show name
    Int value -> show value
    Bool value -> if value then "true" else "false"
    Text value -> show value
    Vec values -> "[" ++ showVector values ++ "]"
    Tuple values -> "(" ++ showVector values ++ ")"
    Fun terms -> "{" ++ unwords (map show terms) ++ "}"
    Closure names terms -> concat
      [ "$("
      , unwords $ map show names
      , "){"
      , unwords $ map show terms
      , "}"
      ]
    Closure' values terms -> concat
      [ "$("
      , unwords $ map show values
      , "){"
      , unwords $ map show terms
      , "}"
      ]
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
resolve prelude (Fragment annos defs terms) = flip evalStateT env0
  $ Fragment annos
  <$> resolveDefs defs
  <*> mapM resolveTerm terms
  where env0 = Env prelude defs []

resolveDefs :: [Def Term] -> Resolution [Def Resolved]
resolveDefs defs = (<>) <$> gets envPrelude <*> mapM resolveDef defs
  where resolveDef (Def name body) = Def name <$> resolveTerm body

resolveTerm :: Term -> Resolution Resolved
resolveTerm unresolved = case unresolved of
  Term.Push value _ -> resolveValue value
  Term.Builtin name loc -> return $ Builtin name loc
  Term.Compose terms -> Compose
    <$> mapM resolveTerm terms
  Term.Lambda name term -> withLocal name
    $ Scoped . list <$> resolveTerm term

fromValue :: Resolved -> Value
fromValue (Push value _) = value
fromValue _ = error "Resolve.fromValue: not a value"

resolveValue :: Term.Value -> Resolution Resolved
resolveValue unresolved = case unresolved of
  Term.Word name loc -> do
    mLocalIndex <- gets $ localIndex name
    case mLocalIndex of
      Just index -> return $ Local (Name index) loc
      Nothing -> do
        mDefIndex <- gets $ defIndex name
        case mDefIndex of
          Just index -> return
            $ Push (Word $ Name index) loc
          Nothing -> lift . Left . CompileError $ concat
            ["Unable to resolve word '", name, "'"]
  Term.Fun term loc -> Push . Fun
    <$> mapM resolveTerm term <*> pure loc
  Term.Vec terms loc -> Push . Vec
    <$> resolveVector terms <*> pure loc
  Term.Tuple terms loc -> Push . Tuple
    <$> resolveVector terms <*> pure loc
  Term.Int value loc -> return $ Push (Int value) loc
  Term.Bool value loc -> return $ Push (Bool value) loc
  Term.Text value loc -> return $ Push (Text value) loc
  where resolveVector = mapM $ fmap fromValue . resolveValue

localIndex :: String -> Env -> Maybe Int
localIndex name = elemIndex name . envScope

defIndex :: String -> Env -> Maybe Int
defIndex expected Env{..} = findExpected envPrelude
  <|> ((+ length envPrelude) <$> findExpected envDefs)
  where findExpected = findIndex $ (== expected) . defName
