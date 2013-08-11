module Kitten.Infer.Type
  ( fromAnno
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List
import Data.Map (Map)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Kitten.Anno (Anno(Anno))
import Kitten.Infer.Monad (Inferred, freshVarM)
import Kitten.Name
import Kitten.Type

import qualified Kitten.Anno as Anno

data Env = Env
  { envRows :: [Name]
  , envScalars :: Map String Name
  , envEffects :: Map String Name
  }

type Converted a = StateT Env Inferred a

fromAnno :: Anno -> Inferred Scheme
fromAnno (Anno annoType loc) = do
  (type_, env) <- flip runStateT Env
    { envRows = []
    , envScalars = Map.empty
    , envEffects = Map.empty
    } $ fromAnnoType' annoType
  return $ Forall
    (Set.fromList (map row (envRows env)))
    (Set.fromList . map scalar . Map.elems $ envScalars env)
    (Set.fromList . map effect . Map.elems $ envEffects env)
    type_
  where

  fromAnnoType' :: Anno.Type -> Converted (Type Scalar)
  fromAnnoType' type_ = case type_ of
    Anno.Bool -> return (Bool loc)
    Anno.Char -> return (Char loc)
    Anno.Choice a b -> (:|) <$> fromAnnoType' a <*> fromAnnoType' b
    Anno.Function a b e -> do
      Var r _ <- lift freshVarM
      modify $ \ env -> env { envRows = r : envRows env }
      Function
        <$> (foldl' (:.) (Var r loc) <$> mapM fromAnnoType' a)
        <*> (foldl' (:.) (Var r loc) <$> mapM fromAnnoType' b)
        <*> fromAnnoEffect e
        <*> pure loc
    Anno.Float -> return (Float loc)
    Anno.Handle -> return (Handle loc)
    Anno.Int -> return (Int loc)
    Anno.Named name -> return (Named name loc)
    Anno.Option a -> (:?) <$> fromAnnoType' a
    Anno.Pair a b -> (:&) <$> fromAnnoType' a <*> fromAnnoType' b
    Anno.Unit -> return (Unit loc)
    Anno.Var name -> do
      mExisting <- gets
        $ \ env -> Map.lookup name (envScalars env)
      case mExisting of
        Just existing -> return (Var existing loc)
        Nothing -> do
          Var var _ <- lift freshVarM
          modify $ \ env -> env
            { envScalars = Map.insert name var (envScalars env) }
          return (Var var loc)
    Anno.Vector a -> Vector <$> fromAnnoType' a <*> pure loc
    _ -> error "converting effect annotation to non-effect type"

  fromAnnoEffect :: Anno.Type -> Converted (Type Effect)
  fromAnnoEffect e = case e of
    Anno.NoEffect -> return (NoEffect loc)
    Anno.IOEffect -> return (IOEffect loc)
    Anno.Var name -> do
      mExisting <- gets
        $ \ env -> Map.lookup name (envEffects env)
      case mExisting of
        Just existing -> return (Var existing loc)
        Nothing -> do
          Var var _ <- lift freshVarM
          modify $ \ env -> env
            { envEffects = Map.insert name var (envEffects env) }
          return (Var var loc)
    Anno.Join a b -> (+:) <$> fromAnnoEffect a <*> fromAnnoEffect b
    _ -> error "converting non-effect annotation to effect type"
