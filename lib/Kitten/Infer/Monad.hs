{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Infer.Monad
  ( Declare(..)
  , Fresh(..)
  , freshConst
  , freshM
  , freshVar
  , freshConstM
  , freshVarM
  , retrieve
  , withLocation
  ) where

import Data.Text (Text)

import qualified Data.Text as T

import Kitten.Error
import Kitten.Id
import Kitten.Kind
import Kitten.KindedId
import Kitten.Location
import Kitten.Program
import Kitten.Type
import Kitten.Util.Maybe
import Kitten.Util.Text (ToText(..))

import qualified Kitten.IdMap as Id

class Declare a where
  declare :: KindedId a -> Type a -> Program -> Program

instance Declare Stack where
  declare (KindedId i) type_ program = program
    { inferenceStacks = Id.insert i type_ (inferenceStacks program) }

instance Declare Scalar where
  declare (KindedId i) type_ program = program
    { inferenceScalars = Id.insert i type_ (inferenceScalars program) }

class Retrieve a where
  retrieve :: Program -> KindedId a -> Either ErrorGroup (Type a)

instance Retrieve Stack where
  retrieve Program{..} (KindedId name)
    = flip maybeToEither (Id.lookup name inferenceStacks)
    $ nonexistent "stack" inferenceLocation name

instance Retrieve Scalar where
  retrieve Program{..} (KindedId name)
    = flip maybeToEither (Id.lookup name inferenceScalars)
    $ nonexistent "scalar" inferenceLocation name

nonexistent :: Text -> Location -> Id n -> ErrorGroup
nonexistent kind loc name
  = oneError $ CompileError loc Error $ T.concat
  [ "nonexistent ", kind, " variable '"
  , toText name
  , "'"
  ]

class Fresh (a :: Kind) where
  fresh
    :: forall (b :: Kind -> *)
    . (KindedId a -> Location -> b a)
    -> Location -> Program -> (b a, Program)

instance Fresh Scalar where
  fresh constructor loc program
    = let (typeId, program') = freshScalarId program
    in (constructor typeId loc, program')

instance Fresh Stack where
  fresh constructor loc program
    = let (typeId, program') = freshStackId program
    in (constructor typeId loc, program')

freshConst
  :: forall (a :: Kind). (Fresh a) => Location -> Program -> (Type a, Program)
freshConst = fresh TyConst

freshVar
  :: forall (a :: Kind). (Fresh a) => Location -> Program -> (Type a, Program)
freshVar = fresh TyVar

freshConstM :: forall (a :: Kind). (Fresh a) => K (Type a)
freshConstM = freshM freshConst

freshVarM :: forall (a :: Kind). (Fresh a) => K (Type a)
freshVarM = freshM freshVar

withLocation :: Location -> K a -> K a
withLocation here action = do
  there <- getsProgram inferenceLocation
  modifyProgram $ \program -> program { inferenceLocation = here }
  result <- action
  modifyProgram $ \program -> program { inferenceLocation = there }
  return result
