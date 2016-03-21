{-# LANGUAGE OverloadedStrings #-}

module Kitten.TypeEnv
  ( TypeEnv(..)
  , empty
  , freshTv
  , freshTypeId
  , getClosed
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import Kitten.Kind (Kind)
import Kitten.Monad (K)
import Kitten.Name
import Kitten.Origin (Origin)
import Kitten.Type (Type(..), TypeId(..), Var(..))
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Text.PrettyPrint as Pretty

-- The typing environment tracks the state of inference. It answers the
-- following questions:
--
--  • What is the type of this type variable?
--  • What is the type of this local variable?
--  • What are the types of the current closure?
--  • What is the signature of this definition?
--
-- It also provides access to the state of globally unique ID generation.

data TypeEnv = TypeEnv
  { tvs :: !(Map TypeId Type)
  , vs :: [Type]
  , closure :: [Type]
  , sigs :: !(Map Qualified Type)
  , currentType :: !(IORef TypeId)
  }

empty :: TypeEnv
empty = TypeEnv
  { tvs = Map.empty
  , vs = []
  , closure = []
  , sigs = Map.empty
  , currentType = currentTypeId
  }

currentTypeId :: IORef TypeId
currentTypeId = unsafePerformIO (newIORef (TypeId 0))
{-# NOINLINE currentTypeId #-}

freshTv :: TypeEnv -> Origin -> Kind -> K Type
freshTv tenv origin k = TypeVar origin <$> (Var <$> freshTypeId tenv <*> pure k)

freshTypeId :: TypeEnv -> K TypeId
freshTypeId tenv = do
  x <- liftIO $ readIORef $ currentType tenv
  liftIO $ writeIORef (currentType tenv) $ succ x
  return x

instance Pretty TypeEnv where
  pPrint tenv = Pretty.vcat
    $ map (\ (v, t) -> Pretty.hsep [pPrint v, "~", pPrint t])
    $ Map.toList $ tvs tenv

getClosed :: TypeEnv -> Closed -> Type
getClosed tenv name = case name of
  ClosedLocal (LocalIndex index) -> vs tenv !! index
  ClosedClosure (ClosureIndex index) -> closure tenv !! index
