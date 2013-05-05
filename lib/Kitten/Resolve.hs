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
import Data.Foldable (forM_)
import Data.List
import Data.Monoid

import Kitten.Anno (Anno)
import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Error
import Kitten.Location
import Kitten.Name
import Kitten.Fragment
import Kitten.Term (Term)
import Kitten.Util.Applicative

import qualified Kitten.Term as Term

data Resolved
  = Block [Resolved]
  | Builtin Builtin Location
  | Closed Name Location
  | If [Resolved] [Resolved] [Resolved] Location
  | Local Name Location
  | Push Value Location
  | Scoped [Resolved] Location
  deriving (Eq)

instance Show Resolved where
  show resolved = case resolved of
    Block terms -> unwords $ map show terms
    Builtin builtin _ -> show builtin
    Closed (Name index) _ -> "closed" ++ show index
    If condition true false _ -> unwords
      [ "if"
      , unwords $ map show condition
      , "then"
      , unwords $ map show true
      , "else"
      , unwords $ map show false
      ]
    Local (Name index) _ -> "local" ++ show index
    Push value _ -> show value
    Scoped terms _ -> unwords $ "\\" : map show terms

data Value
  = Activation [Value] [Resolved]
  | Bool Bool
  | Closure [Name] [Resolved]
  | Escape Name
  | Function Anno [Resolved]
  | Int Int
  | Text String
  | Vector (Maybe Anno) [Value]
  | Word Name
  deriving (Eq)

instance Show Value where
  show v = case v of

    Activation values terms -> concat
      [ "$("
      , unwords $ map show values
      , "){"
      , unwords $ map show terms
      , "}"
      ]

    Bool value -> if value then "true" else "false"

    Closure names terms -> concat
      [ "$("
      , unwords $ map show names
      , "){"
      , unwords $ map show terms
      , "}"
      ]

    Escape (Name name) -> '`' : show name

    Function anno terms -> concat
      [ "("
      , show anno
      , "){"
      , unwords $ map show terms
      , "}"
      ]

    Int value -> show value

    Text value -> show value

    Vector anno values -> concat
      [ maybe "" (("(" ++) . (++ ")") . show) anno
      , "["
      , showVector values
      , "]"
      ]

    Word (Name name) -> '@' : show name

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
resolve prelude (Fragment defs terms) = do
  -- TODO Don't fail so eagerly.
  resolveDuplicateDefs defs
  flip evalStateT env0 $ Fragment
    <$> resolveDefs defs
    <*> mapM resolveTerm terms
  where env0 = Env prelude defs []

resolveDuplicateDefs
  :: [Def Term]
  -> Either CompileError ()
resolveDuplicateDefs defs = forM_ defs $ \ def
  -> case filter (duplicate def) defs of
    [] -> Right ()
    duplicates -> Left $ DuplicateError
      (defLocation def)
      (map defLocation duplicates)
      (defName def)

  where
  duplicate :: Def Term -> Def Term -> Bool
  duplicate def
    = (== defName def) . defName
    .&&. (/= defLocation def) . defLocation

resolveDefs :: [Def Term] -> Resolution [Def Resolved]
resolveDefs defs = (<>) <$> gets envPrelude <*> mapM resolveDef defs
  where
  resolveDef (Def name body loc)
    = Def name <$> resolveTerm body <*> pure loc

resolveTerm :: Term -> Resolution Resolved
resolveTerm unresolved = case unresolved of
  Term.Push value _ -> resolveValue value
  Term.Builtin name loc -> return $ Builtin name loc
  Term.Block terms -> Block
    <$> mapM resolveTerm terms
  Term.Lambda name terms loc -> withLocal name
    $ Scoped <$> mapM resolveTerm terms <*> pure loc
  Term.If condition true false loc -> If
    <$> mapM resolveTerm condition
    <*> mapM resolveTerm true
    <*> mapM resolveTerm false
    <*> pure loc

fromValue :: Resolved -> Value
fromValue (Push value _) = value
fromValue _ = error "Resolve.fromValue: not a value"

resolveValue :: Term.Value -> Resolution Resolved
resolveValue unresolved = case unresolved of
  Term.Word name loc -> resolveName Word name loc
  Term.Function anno term loc -> Push . Function anno
    <$> mapM resolveTerm term <*> pure loc
  Term.Vector anno terms loc -> Push . Vector anno
    <$> resolveVector terms <*> pure loc
  Term.Escape name loc -> resolveName Escape name loc
  Term.Int value loc -> return $ Push (Int value) loc
  Term.Bool value loc -> return $ Push (Bool value) loc
  Term.Text value loc -> return $ Push (Text value) loc
  where
  resolveVector = mapM $ fmap fromValue . resolveValue

resolveName
  :: (Name -> Value)
  -> String
  -> Location
  -> Resolution Resolved
resolveName wrap name loc = do
  mLocalIndex <- gets $ localIndex name
  case mLocalIndex of
    Just index -> return $ Local (Name index) loc
    Nothing -> do
      mDefIndex <- gets $ defIndex name
      case mDefIndex of
        Just index -> return
          $ Push (wrap $ Name index) loc
        Nothing -> lift . Left . CompileError loc $ concat
          ["unable to resolve word '", name, "'"]

localIndex :: String -> Env -> Maybe Int
localIndex name = elemIndex name . envScope

defIndex :: String -> Env -> Maybe Int
defIndex expected Env{..} = findExpected envPrelude
  <|> ((+ length envPrelude) <$> findExpected envDefs)
  where findExpected = findIndex $ (== expected) . defName
