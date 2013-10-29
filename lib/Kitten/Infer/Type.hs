{-# LANGUAGE DataKinds #-}

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

import Kitten.Anno (Anno(Anno))
import Kitten.Infer.Monad (Inferred, freshNameM)
import Kitten.Location
import Kitten.Type

import qualified Kitten.Anno as Anno

data Env = Env
  { envAnonRows :: [TypeName Row]
  -- ^ Anonymous rows implicit on both sides of an
  -- 'Anno.Function' constructor.

  , envEffects :: !(Map Text (TypeName Effect))
  -- ^ Map from effect variable names to effect variables
  -- themselves.

  , envRows :: !(Map Text (TypeName Row))
  -- ^ Map from row variable names to row variables
  -- themselves.

  , envScalars :: !(Map Text (TypeName Scalar))
  -- ^ Map from scalar variable names to scalar variables
  -- themselves.
  }

type Converted a = StateT Env Inferred a

fromAnno :: Anno -> Inferred (Scheme (Type Scalar))
fromAnno (Anno annoType loc) = do
  (type_, env) <- flip runStateT Env
    { envAnonRows = []
    , envEffects = M.empty
    , envRows = M.empty
    , envScalars = M.empty
    } $ fromAnnoType' annoType
  return $ Forall
    (S.fromList (envAnonRows env <> M.elems (envRows env)))
    (S.fromList . M.elems $ envScalars env)
    (S.fromList . M.elems $ envEffects env)
    type_
  where

  fromAnnoType' :: Anno.Type -> Converted (Type Scalar)
  fromAnnoType' type_ = case type_ of
    Anno.Bool -> return (Bool loc)
    Anno.Char -> return (Char loc)
    Anno.Choice a b -> (:|) <$> fromAnnoType' a <*> fromAnnoType' b
    Anno.Function a b e -> do
      r <- lift freshNameM
      modify $ \env -> env { envAnonRows = r : envAnonRows env }
      makeFunction (Var r loc) a (Var r loc) b e
    Anno.Float -> return (Float loc)
    Anno.Handle -> return (Handle loc)
    Anno.Int -> return (Int loc)
    Anno.Named name -> return (Named name loc)
    Anno.Option a -> (:?) <$> fromAnnoType' a
    Anno.Pair a b -> (:&) <$> fromAnnoType' a <*> fromAnnoType' b
    Anno.RowFunction leftRow leftScalars rightRow rightScalars effectAnno -> do
      leftRowVar <- rowVar leftRow loc
      rightRowVar <- rowVar rightRow loc
      makeFunction leftRowVar leftScalars rightRowVar rightScalars effectAnno
    Anno.Unit -> return (Unit loc)
    Anno.Var name -> scalarVar name loc
    Anno.Vector a -> Vector <$> fromAnnoType' a <*> pure loc
    _ -> error "converting effect annotation to non-effect type"

  makeFunction
    :: Type Row
    -> Vector Anno.Type
    -> Type Row
    -> Vector Anno.Type
    -> Anno.Type
    -> Converted (Type Scalar)
  makeFunction leftRow leftScalars rightRow rightScalars effectAnno = Function
    <$> (V.foldl' (:.) leftRow <$> V.mapM fromAnnoType' leftScalars)
    <*> (V.foldl' (:.) rightRow <$> V.mapM fromAnnoType' rightScalars)
    <*> fromAnnoEffect effectAnno
    <*> pure loc

  fromAnnoEffect :: Anno.Type -> Converted (Type Effect)
  fromAnnoEffect e = case e of
    Anno.NoEffect -> return (NoEffect loc)
    Anno.IOEffect -> return (IOEffect loc)
    Anno.Var name -> effectVar name loc
    Anno.Join a b -> (+:) <$> fromAnnoEffect a <*> fromAnnoEffect b
    _ -> error "converting non-effect annotation to effect type"

-- | Gets a scalar variable by name from the environment.
scalarVar :: Text -> Location -> Converted (Type Scalar)
scalarVar = annoVar
  (\name -> M.lookup name . envScalars)
  (\name var env -> env
    { envScalars = M.insert name var (envScalars env) })

-- | Gets a row variable by name from the environment.
rowVar :: Text -> Location -> Converted (Type Row)
rowVar = annoVar
  (\name -> M.lookup name . envRows)
  (\name var env -> env
    { envRows = M.insert name var (envRows env) })

-- | Gets an effect variable by name from the environment.
effectVar :: Text -> Location -> Converted (Type Effect)
effectVar = annoVar
  (\name -> M.lookup name . envEffects)
  (\name var env -> env
    { envEffects = M.insert name var (envEffects env) })

-- | Gets a variable by name from the environment, creating
-- it if it does not exist.
annoVar
  :: (Text -> Env -> Maybe (TypeName a))
  -> (Text -> TypeName a -> Env -> Env)
  -> Text
  -> Location
  -> Converted (Type a)
annoVar retrieve save name loc = do
  mExisting <- gets $ retrieve name
  case mExisting of
    Just existing -> return (Var existing loc)
    Nothing -> do
      var <- lift freshNameM
      modify $ save name var
      return (Var var loc)
