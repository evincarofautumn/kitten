{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Kitten.Error
import Kitten.Infer.Monad (Inferred, freshNameM, liftFailWriter)
import Kitten.Location
import Kitten.Type
import Kitten.Util.FailWriter

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
    Anno.Quantified stacks scalars type_ -> do
      stackVars <- V.mapM declareStack stacks
      scalarVars <- V.mapM declareScalar scalars
      scheme <- Forall
        (S.fromList (V.toList stackVars))
        (S.fromList (V.toList scalarVars))
        <$> fromAnnoType' NoHint type_
      return $ Quantified scheme origin
      where
      declareScalar name = do
        var <- lift freshNameM
        modify $ \env -> env
          { envScalars = M.insert name var (envScalars env) }
        return var
      declareStack name = do
        var <- lift freshNameM
        modify $ \env -> env
          { envStacks = M.insert name var (envStacks env) }
        return var
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
scalarVar
  :: Text -> Location -> Annotated -> Converted (Type Scalar)
scalarVar name loc annotated = do
  existing <- gets $ M.lookup name . envScalars
  case existing of
    Just var -> return $ Var var (Origin (AnnoVar name annotated) loc)
    Nothing -> unknown name loc

-- | Gets a stack variable by name from the environment.
stackVar
  :: Text -> Location -> Annotated -> Converted (Type Stack)
stackVar name loc annotated = do
  existing <- gets $ M.lookup name . envStacks
  case existing of
    Just var -> return $ Var var (Origin (AnnoVar name annotated) loc)
    Nothing -> unknown name loc

unknown :: Text -> Location -> Converted a
unknown name loc = lift . liftFailWriter . throwMany . (:[]) $ ErrorGroup
  [ CompileError loc Error
    $ "unknown type or undeclared type variable " <> name
  ]
