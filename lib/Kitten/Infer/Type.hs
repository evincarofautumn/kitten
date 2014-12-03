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

  , envStacks :: !(HashMap Name (KindedId Stack, Location))
  -- ^ Map from stack variable names to stack variables
  -- themselves.

  , envScalars :: !(HashMap Name (KindedId Scalar, Location))
  -- ^ Map from scalar variable names to scalar variables
  -- themselves.

  , envTypeDefs :: !(HashMap Name TypeDef)

  , envVocabulary :: !Qualifier
  }

type Converted a = StateT Env K a

fromAnno
  :: Qualifier
  -> HashMap Name TypeDef
  -> HashMap (Qualifier, Text) Qualifier
  -> Anno
  -> K (Scheme (Type Scalar))
fromAnno vocab typeDefNames newAbbrevs (Anno annoType) = do
  oldAbbrevs <- getsProgram programAbbrevs
  (type_, env) <- flip runStateT Env
    { envAbbrevs = oldAbbrevs <> newAbbrevs
    , envAnonStacks = []
    , envStacks = H.empty
    , envScalars = H.empty
    , envTypeDefs = typeDefNames
    , envVocabulary = vocab
    } $ fromAnnoType' annoType

  return $ Forall
    (S.fromList (envAnonStacks env <> map fst (H.elems (envStacks env))))
    (S.fromList . map fst . H.elems $ envScalars env)
    type_
  where

  fromAnnoType' :: AnType -> Converted (Type Scalar)
  fromAnnoType' = \case
    AnApp a bs loc -> TyApply
      <$> fromAnnoType' a
      <*> V.mapM fromAnnoType' bs
      <*> pure loc
    AnChoice a b loc -> TySum
      <$> fromAnnoType' a
      <*> fromAnnoType' b
      <*> pure loc
    AnFunction a b loc -> do
      r <- lift freshStackIdM
      let rVar = TyVar r loc
      scheme <- Forall (S.singleton r) S.empty
        <$> makeFunction loc rVar a rVar b
      return $ TyQuantified scheme loc
    AnOption a loc -> TyOption
      <$> fromAnnoType' a
      <*> pure loc
    AnPair a b loc -> TyProduct
      <$> fromAnnoType' a
      <*> fromAnnoType' b
      <*> pure loc
    AnQuantified stacks scalars type_ quantifiedLoc -> do
      stackVars <- V.mapM declareStack stacks
      scalarVars <- V.mapM declareScalar scalars
      scheme <- Forall
        (S.fromList (V.toList stackVars))
        (S.fromList (V.toList scalarVars))
        <$> fromAnnoType' type_
      return $ TyQuantified scheme quantifiedLoc
      where
      declareScalar (name, loc) = do
        var <- lift freshScalarIdM
        modify $ \env -> env
          { envScalars = H.insert name (var, loc) (envScalars env) }
        return var
      declareStack (name, loc) = do
        var <- lift freshStackIdM
        modify $ \env -> env
          { envStacks = H.insert name (var, loc) (envStacks env) }
        return var
    AnStackFunction leftStack leftScalars rightStack rightScalars loc -> do
      leftStackVar <- annoStackVar leftStack loc
      rightStackVar <- annoStackVar rightStack loc
      makeFunction loc leftStackVar leftScalars rightStackVar rightScalars
    AnVar name loc -> annoScalarVar name loc
    AnVector a loc -> TyVector <$> fromAnnoType' a <*> pure loc

  makeFunction
    :: Location
    -> Type Stack
    -> Vector AnType
    -> Type Stack
    -> Vector AnType
    -> Converted (Type Scalar)
  makeFunction loc leftStack leftScalars rightStack rightScalars = TyFunction
    <$> (V.foldl' TyStack leftStack <$> V.mapM fromAnnoType' leftScalars)
    <*> (V.foldl' TyStack rightStack <$> V.mapM fromAnnoType' rightScalars)
    <*> pure loc

fromAnno _ _ _ TestAnno = error "cannot make type from test annotation"

-- | Gets a scalar variable by name from the environment.
annoScalarVar :: Name -> Location -> Converted (Type Scalar)
annoScalarVar name loc = do
  existing <- gets $ H.lookup name . envScalars
  vocab <- gets envVocabulary
  abbrevs <- gets envAbbrevs
  let lookupAbbrev x = H.lookup x abbrevs
  case existing of
    Just (var, existingLoc) -> return $ TyVar var existingLoc
    Nothing -> case name of
      Unqualified text
        | text == "bool" -> return $ TyCtor CtorBool loc
        | text == "char" -> return $ TyCtor CtorChar loc
        | text == "float" -> return $ TyCtor CtorFloat loc
        | text == "handle" -> return $ TyCtor CtorHandle loc
        | text == "int" -> return $ TyCtor CtorInt loc
        | otherwise -> search vocab text isDefined call (unknown name loc)
      Qualified qualifier text -> searchAbbrev
        vocab qualifier text isDefined lookupAbbrev call (unknown name loc)
      _ -> unknown name loc
  where
  isDefined x = gets $ isJust . H.lookup x . envTypeDefs
  call x = return $ TyCtor (CtorUser x) loc

-- | Gets a stack variable by name from the environment.
annoStackVar :: Name -> Location -> Converted (Type Stack)
annoStackVar name loc = do
  existing <- gets $ H.lookup name . envStacks
  case existing of
    Just (var, existingLoc) -> return $ TyVar var existingLoc
    Nothing -> unknown name loc

unknown :: Name -> Location -> Converted a
unknown name loc = lift . liftFailWriter . throwMany . (:[]) $ ErrorGroup
  [ CompileError loc Error
    $ "unknown type or undeclared type variable " <> toText name
  ]
