{-# LANGUAGE RecordWildCards #-}

module Kitten.Infer.Type
  ( toTypedDef
  , toTypedFragment
  , toTypedTerm
  , fromAnno
  ) where

import Control.Applicative
import Control.Monad.Trans.State
import Data.List

import qualified Data.Set as Set

import Kitten.Anno (Anno(Anno))
import Kitten.Def
import Kitten.Fragment
import Kitten.Infer.Monad
import Kitten.Name
import Kitten.Resolved (Resolved)
import Kitten.Type
import Kitten.Typed

import qualified Kitten.Anno as Anno
import qualified Kitten.Resolved as Resolved

toTypedDef
  :: Def Resolved.Value
  -> Inferred (Def Value)
toTypedDef def@Def{..} = do
  defTerm' <- toTypedValue defTerm
  return def { defTerm = defTerm' }

toTypedFragment
  :: Fragment Resolved.Value Resolved
  -> Inferred (Fragment Value Typed)
toTypedFragment fragment@Fragment{..} = do
  fragmentDefs' <- mapM toTypedDef fragmentDefs
  fragmentTerms' <- mapM toTypedTerm fragmentTerms
  return fragment
    { fragmentDefs = fragmentDefs'
    , fragmentTerms = fragmentTerms'
    }

toTypedTerm :: Resolved -> Inferred Typed
toTypedTerm resolved = case resolved of
  Resolved.Block terms -> compose terms
  Resolved.Builtin builtin loc
    -> pure $ Builtin builtin loc
  Resolved.Call name loc -> pure $ Call name loc
  Resolved.If condition true false loc -> do
    condition' <- mapM toTypedTerm condition
    typed <- If
      <$> compose true
      <*> compose false
      <*> pure loc
    return $ Compose (condition' ++ [typed])
  Resolved.PairTerm as bs loc -> PairTerm
    <$> compose as
    <*> compose bs
    <*> pure loc
  Resolved.Push value loc -> Push
    <$> toTypedValue value
    <*> pure loc
  Resolved.Scoped terms loc -> Scoped
    <$> compose terms
    <*> pure loc
  Resolved.VectorTerm terms loc -> VectorTerm
    <$> mapM compose terms
    <*> pure loc

toTypedValue :: Resolved.Value -> Inferred Value
toTypedValue resolved = case resolved of
  Resolved.Activation closure terms -> Activation
    <$> mapM toTypedValue closure
    <*> compose terms
  Resolved.Bool bool -> pure $ Bool bool
  Resolved.Char char -> pure $ Char char
  Resolved.Closed name
    -> pure $ Closed name
  Resolved.Closure names terms
    -> Closure names <$> compose terms
  Resolved.Float float -> pure $ Float float
  Resolved.Function terms -> Function <$> compose terms
  Resolved.Handle handle -> pure $ Handle handle
  Resolved.Int int -> pure $ Int int
  Resolved.Local name -> pure $ Local name
  Resolved.Pair a b
    -> Pair <$> toTypedValue a <*> toTypedValue b
  Resolved.Unit -> pure Unit
  Resolved.Vector values -> Vector
    <$> mapM toTypedValue values

compose :: [Resolved] -> Inferred Typed
compose terms = Compose <$> mapM toTypedTerm terms

fromAnno :: Anno -> Inferred Scheme
fromAnno (Anno annoType _) = do
  Name startIndex <- getsEnv envNext
  let
    (count, type_) = fromAnnoType startIndex annoType
    scheme = Forall
      (Set.fromList (map (Name . (startIndex +)) [0 .. pred count]))
      type_
  modifyEnv $ \ env -> env { envNext = Name (nameIndex (envNext env) + count) }
  return scheme

fromAnnoType :: Int -> Anno.Type -> (Int, Type)
fromAnnoType startIndex annoType
  = let (type_, names) = flip runState [] $ fromAnnoType' annoType
  in (length names, type_)

  where
  fromAnnoType' :: Anno.Type -> State [String] Type
  fromAnnoType' type_ = case type_ of

    a Anno.:> b -> (:>)
      <$> mapM fromAnnoType' a
      <*> mapM fromAnnoType' b

    Anno.Bool -> return BoolType

    Anno.Char -> return CharType

    Anno.Float -> return FloatType

    Anno.Handle -> return HandleType

    Anno.Int -> return IntType

    Anno.Pair a b -> (:&) <$> fromAnnoType' a <*> fromAnnoType' b

    Anno.Unit -> return UnitType

    Anno.Var name -> do
      mExisting <- gets $ elemIndex name
      case mExisting of
        Just existing -> return $ TypeVar (Name (startIndex + existing))
        Nothing -> do
          index <- gets length
          modify (++ [name])
          return $ TypeVar (Name (startIndex + index))

    Anno.Vector a -> VectorType <$> fromAnnoType' a
