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
  }

fromAnno :: Anno -> Inferred Scheme
fromAnno (Anno annoType _) = do
  (type_, env) <- flip runStateT Env
    { envRows = []
    , envScalars = Map.empty
    } $ fromAnnoType' annoType
  return $ Forall
    (Set.fromList (map row (envRows env)))
    (Set.fromList . map scalar . Map.elems $ envScalars env)
    type_
  where
  fromAnnoType'
    :: Anno.Type
    -> StateT Env Inferred (Type Scalar)
  fromAnnoType' type_ = case type_ of
    Anno.Bool -> return Bool
    Anno.Char -> return Char
    Anno.Choice a b -> (:|) <$> fromAnnoType' a <*> fromAnnoType' b
    Anno.Function a b p -> do
      Var r <- lift freshVarM
      modify $ \ env -> env { envRows = r : envRows env }
      Function
        <$> (foldl' (:.) (Var r) <$> mapM fromAnnoType' a)
        <*> (foldl' (:.) (Var r) <$> mapM fromAnnoType' b)
        <*> pure p
    Anno.Float -> return Float
    Anno.Handle -> return Handle
    Anno.Int -> return Int
    Anno.Option a -> (:?) <$> fromAnnoType' a
    Anno.Pair a b -> (:&) <$> fromAnnoType' a <*> fromAnnoType' b
    Anno.Unit -> return Unit
    Anno.Var name -> do
      mExisting <- gets
        $ \ env -> Map.lookup name (envScalars env)
      case mExisting of
        Just existing -> return (Var existing)
        Nothing -> do
          Var var <- lift freshVarM
          modify $ \ env -> env
            { envScalars = Map.insert name var (envScalars env) }
          return (Var var)
    Anno.Vector a -> Vector <$> fromAnnoType' a
