{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Kitten.K
  ( Inference(..)
  , K
  , Program(..)
  , freshDefId
  , freshDefIdM
  , freshTypeId
  , freshTypeIdM
  , freshM
  , getInference
  , getProgram
  , getsInference
  , getsProgram
  , emptyInference
  , emptyProgram
  , liftState
  , modifyInference
  , modifyProgram
  , putInference
  , putProgram
  , runK
  ) where

import Control.Applicative
import Control.Monad.Trans.State
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import Text.Parsec.Pos

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Kitten.Id
import Kitten.IdMap (IdMap)
import Kitten.Location
import Kitten.Type

import qualified Kitten.IdMap as Id

data Program a = Program
  { programBlocks :: !(IdMap DefSpace a)
  , programDefIdGen :: !(IdGen DefSpace)
  , programTypeIdGen :: !(IdGen TypeSpace)
  , programInference :: !Inference
  , programSymbols :: !(HashMap Text (Id DefSpace))
  }

data Inference = Inference
  { inferenceClosure :: !(Vector (Type Scalar))
  , inferenceDefs :: !(HashMap Text TypeScheme)
  , inferenceDecls :: !(HashMap Text TypeScheme)
  , inferenceLocals :: [Type Scalar]
  , inferenceOrigin :: !Origin
  , inferenceScalars :: !(IdMap TypeSpace (Type Scalar))
  , inferenceStacks :: !(IdMap TypeSpace (Type Stack))
  }

newtype K block a = K
  { unwrapK :: StateT (Program block) IO a }
  deriving (Applicative, Functor, Monad)

getInference :: K block Inference
getInference = K (gets programInference)

getProgram :: K block (Program block)
getProgram = K get

getsInference :: (Inference -> a) -> K block a
getsInference = K . gets . (. programInference)

getsProgram :: (Program block -> a) -> K block a
getsProgram = K . gets

liftState :: StateT (Program block) IO a -> K block a
liftState = K

modifyInference :: (Inference -> Inference) -> K block ()
modifyInference f = modifyProgram $ \p -> p
  { programInference = f (programInference p) }

modifyProgram :: (Program block -> Program block) -> K block ()
modifyProgram = K . modify

putInference :: Inference -> K block ()
putInference = modifyInference . const

putProgram :: Program block -> K block ()
putProgram = K . put

runK :: (Monoid block) => K block a -> IO (a, Program block)
runK = flip runStateT emptyProgram . unwrapK

emptyProgram :: (Monoid block) => Program block
emptyProgram = Program
  { programBlocks = Id.singleton entryId mempty
  , programDefIdGen = mkIdGenFrom (succ entryId)
  , programTypeIdGen = mkIdGen
  , programInference = emptyInference
  , programSymbols = H.empty
  }

entryId :: Id DefSpace
entryId = Id 0

emptyInference :: Inference
emptyInference = Inference
  { inferenceClosure = V.empty
  , inferenceDefs = H.empty
  , inferenceDecls = H.empty
  , inferenceLocals = []
  , inferenceOrigin = Origin NoHint Location
    { locationStart = initialPos "<unknown>"
    , locationIndent = 0
    }
  , inferenceScalars = Id.empty
  , inferenceStacks = Id.empty
  }

freshDefId :: Program block -> (Id DefSpace, Program block)
freshDefId program
  = let (i, gen') = genId (programDefIdGen program)
  in (i, program { programDefIdGen = gen' })

freshDefIdM :: K block (Id DefSpace)
freshDefIdM = K $ state freshDefId

freshTypeId :: Program block -> (Id TypeSpace, Program block)
freshTypeId program
  = let (i, gen') = genId (programTypeIdGen program)
  in (i, program { programTypeIdGen = gen' })

freshTypeIdM :: K block (Id TypeSpace)
freshTypeIdM = K $ state freshTypeId

freshM
  :: forall a (b :: Kind -> *) block
  . (Origin -> Program block -> (b a, Program block))
  -> K block (b a)
freshM action = do
  Origin hint loc <- getsProgram (inferenceOrigin . programInference)
  K $ state (action (Origin hint loc))
