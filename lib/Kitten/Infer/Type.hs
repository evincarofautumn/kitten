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
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Vector as V

import Kitten.Annotation
import Kitten.Error
import Kitten.Kind
import Kitten.KindedId
import Kitten.Location
import Kitten.Name
import Kitten.Program
import Kitten.Resolve
import Kitten.Type
import Kitten.TypeDefinition
import Kitten.Util.FailWriter
import Kitten.Util.Text (ToText(..))

data Env = Env
  { envAbbrevs :: !(HashMap (Qualifier, Text) Qualifier)

  , envAnonStacks :: [KindedId Stack]
  -- ^ Anonymous stacks implicit on both sides of an
  -- 'Anno.Function' constructor.

  , envStacks :: !(HashMap Name (KindedId Stack))
  -- ^ Map from stack variable names to stack variables
  -- themselves.

  , envScalars :: !(HashMap Name (KindedId Scalar))
  -- ^ Map from scalar variable names to scalar variables
  -- themselves.

  , envTypeDefs :: !(HashMap Name TypeDef)

  , envVocabulary :: !Qualifier
  }

type Converted a = StateT Env K a

fromAnno
  :: Annotated
  -> Qualifier
  -> HashMap Name TypeDef
  -> HashMap (Qualifier, Text) Qualifier
  -> Anno
  -> K (Scheme (Type Scalar))
fromAnno annotated vocab typeDefNames newAbbrevs (Anno annoType annoLoc) = do
  oldAbbrevs <- getsProgram programAbbrevs
  (type_, env) <- flip runStateT Env
    { envAbbrevs = oldAbbrevs <> newAbbrevs
    , envAnonStacks = []
    , envStacks = H.empty
    , envScalars = H.empty
    , envTypeDefs = typeDefNames
    , envVocabulary = vocab
    } $ fromAnnoType' (HiType annotated) annoType

  return $ Forall
    (S.fromList (envAnonStacks env <> H.elems (envStacks env)))
    (S.fromList . H.elems $ envScalars env)
    type_
  where

  fromInput, fromOutput :: AnType -> Converted (Type Scalar)
  fromInput = fromAnnoType' (HiFunctionInput annotated)
  fromOutput = fromAnnoType' (HiFunctionOutput annotated)

  fromAnnoType' :: Hint -> AnType -> Converted (Type Scalar)
  fromAnnoType' hint = \case
    AnApp a bs -> (:@)
      <$> fromAnnoType' HiNone a
      <*> V.mapM (fromAnnoType' HiNone) bs
    AnChoice a b -> (:|)
      <$> fromAnnoType' HiNone a
      <*> fromAnnoType' HiNone b
    AnFunction a b -> do
      r <- lift freshStackIdM
      let rVar = TyVar r origin
      scheme <- Forall (S.singleton r) S.empty
        <$> makeFunction origin rVar a rVar b
      return $ TyQuantified scheme origin
    AnOption a -> (:?)
      <$> fromAnnoType' HiNone a
    AnPair a b -> (:&)
      <$> fromAnnoType' HiNone a
      <*> fromAnnoType' HiNone b
    AnQuantified stacks scalars type_ -> do
      stackVars <- V.mapM declareStack stacks
      scalarVars <- V.mapM declareScalar scalars
      scheme <- Forall
        (S.fromList (V.toList stackVars))
        (S.fromList (V.toList scalarVars))
        <$> fromAnnoType' HiNone type_
      return $ TyQuantified scheme origin
      where
      declareScalar name = do
        var <- lift freshScalarIdM
        modify $ \env -> env
          { envScalars = H.insert name var (envScalars env) }
        return var
      declareStack name = do
        var <- lift freshStackIdM
        modify $ \env -> env
          { envStacks = H.insert name var (envStacks env) }
        return var
    AnStackFunction leftStack leftScalars rightStack rightScalars -> do
      leftStackVar <- annoStackVar leftStack loc annotated
      rightStackVar <- annoStackVar rightStack loc annotated
      makeFunction origin leftStackVar leftScalars rightStackVar rightScalars
    AnVar name -> annoScalarVar name loc annotated
    AnVector a -> TyVector <$> fromAnnoType' HiNone a <*> pure origin
    where
    origin :: Origin
    origin = Origin hint loc
    loc :: Location
    loc = annoLoc  -- FIXME(strager)

  makeFunction
    :: Origin
    -> Type Stack
    -> Vector AnType
    -> Type Stack
    -> Vector AnType
    -> Converted (Type Scalar)
  makeFunction origin leftStack leftScalars rightStack rightScalars = TyFunction
    <$> (V.foldl' (:.) leftStack <$> V.mapM fromInput leftScalars)
    <*> (V.foldl' (:.) rightStack <$> V.mapM fromOutput rightScalars)
    <*> pure origin

fromAnno _ _ _ _ TestAnno = error "cannot make type from test annotation"

-- | Gets a scalar variable by name from the environment.
annoScalarVar
  :: Name -> Location -> Annotated -> Converted (Type Scalar)
annoScalarVar name loc annotated = do
  existing <- gets $ H.lookup name . envScalars
  vocab <- gets envVocabulary
  abbrevs <- gets envAbbrevs
  let lookupAbbrev x = H.lookup x abbrevs
  case existing of
    Just var -> return $ TyVar var origin
    Nothing -> case name of
      Unqualified text
        | text == "bool" -> return $ TyCtor CtorBool origin
        | text == "char" -> return $ TyCtor CtorChar origin
        | text == "float" -> return $ TyCtor CtorFloat origin
        | text == "handle" -> return $ TyCtor CtorHandle origin
        | text == "int" -> return $ TyCtor CtorInt origin
        | otherwise -> search vocab text isDefined call (unknown name loc)
      Qualified qualifier text -> searchAbbrev
        vocab qualifier text isDefined lookupAbbrev call (unknown name loc)
      _ -> unknown name loc
  where
  origin = Origin (HiVar name annotated) loc
  isDefined x = gets $ isJust . H.lookup x . envTypeDefs
  call x = return $ TyCtor (CtorUser x) origin

-- | Gets a stack variable by name from the environment.
annoStackVar
  :: Name -> Location -> Annotated -> Converted (Type Stack)
annoStackVar name loc annotated = do
  existing <- gets $ H.lookup name . envStacks
  case existing of
    Just var -> return $ TyVar var (Origin (HiVar name annotated) loc)
    Nothing -> unknown name loc

unknown :: Name -> Location -> Converted a
unknown name loc = lift . liftFailWriter . throwMany . (:[]) $ ErrorGroup
  [ CompileError loc Error
    $ "unknown type or undeclared type variable " <> toText name
  ]
