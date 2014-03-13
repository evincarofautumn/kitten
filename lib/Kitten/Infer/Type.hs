{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Kitten.Infer.Type
  ( fromAnno
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Kitten.Anno (Anno(..))
import Kitten.Infer.Monad (Inferred, freshNameM)
import Kitten.Location
import Kitten.Type

import qualified Kitten.Anno as Anno

data Env = Env
  { envAnonStacks :: [TypeName Stack]
  -- ^ Anonymous stacks implicit on both sides of an
  -- 'Anno.Function' constructor.

  , envStacks :: !(Map Text (TypeName Stack))
  -- ^ Map from stack variable names to stack variables
  -- themselves.

  , envScalars :: !(Map Text (TypeName Scalar))
  -- ^ Map from scalar variable names to scalar variables
  -- themselves.
  }

type Converted a = StateT Env Inferred a

fromAnno :: Annotated -> Anno -> Inferred (Scheme (Type Scalar))
fromAnno annotated (Anno annoType annoLoc) = do
  (type_, env) <- flip runStateT Env
    { envAnonStacks = []
    , envStacks = M.empty
    , envScalars = M.empty
    } $ fromAnnoType' (AnnoType annotated) annoType

  return $ Forall
    (S.fromList (envAnonStacks env <> M.elems (envStacks env)))
    (S.fromList . M.elems $ envScalars env)
    type_
  where

  fromInput, fromOutput :: Anno.Type -> Converted (Type Scalar)
  fromInput = fromAnnoType' (AnnoFunctionInput annotated)
  fromOutput = fromAnnoType' (AnnoFunctionOutput annotated)

  fromAnnoType' :: Hint -> Anno.Type -> Converted (Type Scalar)
  fromAnnoType' hint = \case
    Anno.Bool -> return (Bool origin)
    Anno.Char -> return (Char origin)
    Anno.Choice a b -> (:|)
      <$> fromAnnoType' NoHint a
      <*> fromAnnoType' NoHint b
    Anno.Function a b -> do
      r <- lift freshNameM
      let rVar = Var r origin
      scheme <- Forall (S.singleton r) S.empty
        <$> makeFunction origin rVar a rVar b
      return $ Quantified scheme origin
    Anno.Float -> return (Float origin)
    Anno.Handle -> return (Handle origin)
    Anno.Int -> return (Int origin)
    Anno.Option a -> (:?)
      <$> fromAnnoType' NoHint a
    Anno.Pair a b -> (:&)
      <$> fromAnnoType' NoHint a
      <*> fromAnnoType' NoHint b
    Anno.StackFunction leftStack leftScalars rightStack rightScalars -> do
      leftStackVar <- stackVar leftStack loc annotated
      rightStackVar <- stackVar rightStack loc annotated
      makeFunction origin leftStackVar leftScalars rightStackVar rightScalars
    Anno.Var name -> scalarVar name loc annotated
    Anno.Vector a -> Vector <$> fromAnnoType' NoHint a <*> pure origin
    where
    origin :: Origin
    origin = Origin hint loc
    loc :: Location
    loc = annoLoc  -- FIXME(strager)

  makeFunction
    :: Origin
    -> Type Stack
    -> Vector Anno.Type
    -> Type Stack
    -> Vector Anno.Type
    -> Converted (Type Scalar)
  makeFunction origin leftStack leftScalars rightStack rightScalars = Function
    <$> (V.foldl' (:.) leftStack <$> V.mapM fromInput leftScalars)
    <*> (V.foldl' (:.) rightStack <$> V.mapM fromOutput rightScalars)
    <*> pure origin

fromAnno _ TestAnno = error "cannot make type from test annotation"

-- | Gets a scalar variable by name from the environment.
scalarVar :: Text -> Location -> Annotated -> Converted (Type Scalar)
scalarVar = annoVar
  (\name -> M.lookup name . envScalars)
  (\name var env -> env
    { envScalars = M.insert name var (envScalars env) })

-- | Gets a stack variable by name from the environment.
stackVar :: Text -> Location -> Annotated -> Converted (Type Stack)
stackVar = annoVar
  (\name -> M.lookup name . envStacks)
  (\name var env -> env
    { envStacks = M.insert name var (envStacks env) })

-- | Gets a variable by name from the environment, creating
-- it if it does not exist.
annoVar
  :: (Text -> Env -> Maybe (TypeName a))
  -> (Text -> TypeName a -> Env -> Env)
  -> Text
  -> Location
  -> Annotated
  -> Converted (Type a)
annoVar retrieve save name loc annotated = do
  mExisting <- gets $ retrieve name
  case mExisting of
    Just existing -> return (Var existing origin)
    Nothing -> do
      var <- lift freshNameM
      modify $ save name var
      return (Var var origin)
  where
  origin :: Origin
  origin = Origin (AnnoVar name annotated) loc
