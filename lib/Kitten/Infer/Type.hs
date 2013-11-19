{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Kitten.Infer.Type
  ( fromAnno
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
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

  , envRows :: !(Map Text (TypeName Row))
  -- ^ Map from row variable names to row variables
  -- themselves.

  , envScalars :: !(Map Text (TypeName Scalar))
  -- ^ Map from scalar variable names to scalar variables
  -- themselves.
  }

type Converted a = ReaderT (Type Row) (StateT Env Inferred) a

getDefaultRow :: (Monad m) => ReaderT (Type Row) m (Type Row)
getDefaultRow = ask

fromAnno :: Annotated -> Anno -> Inferred (Scheme (Type Scalar))
fromAnno annotated (Anno annoType annoLoc) = do
  r <- freshNameM
  let defaultRow = Var r (Origin (AnnoType annotated) annoLoc)

  (type_, env) <- flip runStateT Env
    { envAnonRows = [r]
    , envRows = M.empty
    , envScalars = M.empty
    } $ flip runReaderT defaultRow
    $ fromAnnoType' (AnnoType annotated) annoType

  return $ Forall
    (S.fromList (envAnonRows env <> M.elems (envRows env)))
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
      r <- getDefaultRow
      makeFunction origin r a r b
    Anno.Float -> return (Float origin)
    Anno.Handle -> return (Handle origin)
    Anno.Int -> return (Int origin)
    Anno.Named name -> return (Named name origin)
    Anno.Option a -> (:?)
      <$> fromAnnoType' NoHint a
    Anno.Pair a b -> (:&)
      <$> fromAnnoType' NoHint a
      <*> fromAnnoType' NoHint b
    Anno.RowFunction leftRow leftScalars rightRow rightScalars -> do
      leftRowVar <- rowVar leftRow loc annotated
      rightRowVar <- rowVar rightRow loc annotated
      makeFunction origin leftRowVar leftScalars rightRowVar rightScalars
    Anno.Unit -> return (Unit origin)
    Anno.Var name -> scalarVar name loc annotated
    Anno.Vector a -> Vector <$> fromAnnoType' NoHint a <*> pure origin
    where
    origin :: Origin
    origin = Origin hint loc
    loc :: Location
    loc = annoLoc  -- FIXME(strager)

  makeFunction
    :: Origin
    -> Type Row
    -> Vector Anno.Type
    -> Type Row
    -> Vector Anno.Type
    -> Converted (Type Scalar)
  makeFunction origin leftRow leftScalars rightRow rightScalars = Function
    <$> (V.foldl' (:.) leftRow <$> V.mapM fromInput leftScalars)
    <*> (V.foldl' (:.) rightRow <$> V.mapM fromOutput rightScalars)
    <*> pure origin

-- | Gets a scalar variable by name from the environment.
scalarVar :: Text -> Location -> Annotated -> Converted (Type Scalar)
scalarVar = annoVar
  (\name -> M.lookup name . envScalars)
  (\name var env -> env
    { envScalars = M.insert name var (envScalars env) })

-- | Gets a row variable by name from the environment.
rowVar :: Text -> Location -> Annotated -> Converted (Type Row)
rowVar = annoVar
  (\name -> M.lookup name . envRows)
  (\name var env -> env
    { envRows = M.insert name var (envRows env) })

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
  mExisting <- lift $ gets $ retrieve name
  case mExisting of
    Just existing -> return (Var existing origin)
    Nothing -> do
      var <- lift $ lift $ freshNameM
      lift $ modify $ save name var
      return (Var var origin)
  where
  origin :: Origin
  origin = Origin (AnnoVar name annotated) loc
