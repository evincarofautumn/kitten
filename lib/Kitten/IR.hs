{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -w #-}

module Kitten.IR
  ( FlattenedProgram(..)
  , declareBlock
  , emptyProgram
  , entryId
  , flattenProgram
  , inverseSymbols
  , ir
  ) where

import Control.Applicative hiding (some)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Monoid
import Data.Text (Text)
import Data.Traversable (traverse)
import Data.Vector (Vector)
import System.IO

import qualified Data.Char as Char
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Error
import Kitten.Id
import Kitten.IdMap (DefIdMap)
import Kitten.Types
import Kitten.Util.Monad
import Kitten.Util.Text (ToText(..), showText)
import Kitten.Util.Tuple

import qualified Kitten.IdMap as Id

ir :: Fragment TypedTerm -> Program -> Config -> (Either [ErrorGroup] (), Program)
ir Fragment{..} program config = runK program config $ do
  F.mapM_ irDef fragmentDefs
  irEntry fragmentTerm

irDef :: Def TypedTerm -> K ()
irDef Def{..} = do
  defId <- getDefM defName
  block <- terminated <$> irTerm (unscheme defTerm)
  modifyProgram $ \program@Program{..} -> program
    { programBlocks = Id.insert defId block programBlocks }

irEntry :: TypedTerm -> K ()
irEntry term = do
  instructions <- irTerm term
  modifyProgram $ \program@Program{..} -> program
    { programBlocks = Id.adjust (<> instructions) entryId programBlocks }

irTerm :: TypedTerm -> K IrBlock
irTerm term = case term of
  TrCall _ target _ -> do
    target' <- getDefM target
    return $ V.singleton (IrCall target')
  TrCompose _ terms _ -> concatMapM irTerm terms
  TrConstruct _ size _ -> return $ V.singleton (IrConstruct size)
  TrIntrinsic intrinsic _ -> return $ V.singleton (IrIntrinsic intrinsic)
  TrLambda _ terms _ -> do
    instructions <- irTerm terms
    return $ V.singleton IrEnter <> instructions <> V.singleton IrLeave
  TrMakePair a b _ -> do
    a' <- irTerm a
    b' <- irTerm b
    return $ a' <> b' <> V.singleton (IrIntrinsic InPair)
  TrPush value _ -> irValue value
  TrMakeVector values _ -> do
    values' <- concatMapM irTerm values
    return $ values' <> (V.singleton . IrMakeVector $ V.length values)

irValue :: TypedValue -> K IrBlock
irValue resolved = case resolved of
  TrBool x _ -> value $ IrBool x
  TrChar x _ -> value $ IrChar x
  TrClosed index _ -> return $ V.singleton (IrClosure index)
  TrClosure names terms (loc, type_) -> do
    instructions <- irTerm terms
    target <- declareBlockM Nothing (terminated instructions)
    return $ V.singleton (IrAct target names type_)
  TrFloat x _ -> value $ IrFloat x
  TrInt x _ -> value $ IrInt x
  TrLocal index _ -> return $ V.singleton (IrLocal index)
  TrQuotation{} -> error "quotation appeared during conversion to IR"
  TrText x _ -> value $ IrString x
  where
  value :: IrValue -> K IrBlock
  value = return . V.singleton . IrPush

terminated :: IrBlock -> IrBlock
terminated = (<> V.singleton IrReturn)

data FlattenedProgram = FlattenedProgram
  { flattenedBlock :: !IrBlock
  , flattenedNames :: !(DefIdMap Int)
  , flattenedSymbols :: !(DefIdMap [Text])
  }

declareBlockM :: Maybe Text -> IrBlock -> K DefId
declareBlockM name block = liftState $ state (declareBlock name block)

declareBlock :: Maybe Text -> IrBlock -> Program -> (DefId, Program)
declareBlock mSymbol block program@Program{..} = let
  (i, program') = freshDefId program
  in (,) i program'
    { programBlocks = Id.insert i block programBlocks
    , programSymbols = maybe id (`H.insert` i) mSymbol programSymbols
    }

getDefM :: Text -> K DefId
getDefM = liftState . state . getDef

getDef :: Text -> Program -> (DefId, Program)
getDef name program@Program{..} = case H.lookup name programSymbols of
  Just id' -> (id', program)
  Nothing -> let
    (id', program') = freshDefId program
    in (,) id' program'
      { programSymbols = H.insert name id' programSymbols }

inverseSymbols :: Program -> DefIdMap [Text]
inverseSymbols Program{..}
  = foldl' (\symbols (symbol, name)
    -> Id.insertWith (++) name [symbol] symbols) Id.empty
  $ H.toList programSymbols

flattenProgram :: Program -> FlattenedProgram
flattenProgram program@Program{..} = FlattenedProgram{..}
  where
  (flattenedBlock, flattenedNames) = foldl' go mempty (Id.toList instructions)
  flattenedSymbols = inverseSymbols program

  go :: (IrBlock, DefIdMap Int) -> (DefId, IrBlock) -> (IrBlock, DefIdMap Int)
  go (blocks, names) (name, block)
    = (blocks <> block, Id.insert name (V.length blocks) names)

  instructions :: DefIdMap IrBlock
  instructions = Id.adjust terminated entryId programBlocks
