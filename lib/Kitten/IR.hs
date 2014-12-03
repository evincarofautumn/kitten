{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Traversable (traverse)

import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Kitten.Config
import Kitten.Definition
import Kitten.Error
import Kitten.Fragment
import Kitten.Id
import Kitten.IdMap (DefIdMap)
import Kitten.Intrinsic
import Kitten.Name
import Kitten.Program
import Kitten.Term
import Kitten.Type
import Kitten.TypeDefinition
import Kitten.Util.List
import Kitten.Util.Monad

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
  TrConstruct name ctor size (_, TyFunction _ (TyStack _ type_) _) -> do
    name' <- ctorIndex (Just name) ctor
    return $ V.singleton (IrConstruct name' size type_)
  TrConstruct{} -> error "constructor with non-function type"
  TrIntrinsic intrinsic _ -> return $ V.singleton (IrIntrinsic intrinsic)
  TrLambda _ _ terms _ -> do
    instructions <- irTerm terms
    return $ V.singleton (IrEnter 1) <> instructions <> V.singleton (IrLeave 1)
  TrMakePair a b _ -> do
    a' <- irTerm a
    b' <- irTerm b
    return $ a' <> b' <> V.singleton (IrIntrinsic InPair)
  TrMakeVector values _ -> do
    values' <- concatMapM irTerm values
    return $ values' <> (V.singleton . IrMakeVector $ V.length values)
  TrMatch cases mDefault _ -> V.singleton
    <$> (IrMatch <$> V.mapM irCase cases <*> traverse irDefault mDefault)
    where
    irCase (TrCase name (TrClosure names body (_, type_)) _) = do
      instructions <- irTerm body
      target <- declareBlockM Nothing (terminated instructions)
      name' <- ctorIndex Nothing name
      return $ IrCase name' (target, names, type_)
    irCase TrCase{} = error "case with non-closure body"
    irDefault (TrClosure names body (_, type_)) = do
      instructions <- irTerm body
      target <- declareBlockM Nothing (terminated instructions)
      return (target, names, type_)
    irDefault _ = error "default with non-closure body"
  TrPush value _ -> irValue value

ctorIndex :: Maybe Name -> Name -> K Int
ctorIndex mName ctor = case mName of
  Just name -> fromMaybe
    (error $ concat
      [ "using non-constructor '"
      , show ctor
      , "' as constructor of '"
      , show name
      , "'"
      ])
    . (findCtor =<<) . H.lookup name <$> getsProgram programTypes
  Nothing -> fromMaybe
    (error $ "match on non-constructor '" ++ show ctor ++ "'")
    . findMap findCtor . H.elems <$> getsProgram programTypes
  where
  findCtor = V.findIndex ((ctor ==) . ctorName) . typeDefConstructors

irValue :: TypedValue -> K IrBlock
irValue resolved = case resolved of
  TrBool x _ -> value $ IrBool x
  TrChar x _ -> value $ IrChar x
  TrClosed index _ -> return $ V.singleton (IrClosure index)
  TrClosure names terms (_, type_) -> do
    instructions <- irTerm terms
    target <- declareBlockM Nothing (terminated instructions)
    return $ V.singleton (IrAct (target, names, type_))
  TrFloat x _ -> value $ IrFloat x
  TrInt x _ -> value $ IrInt x
  TrLocal index _ -> return $ V.singleton (IrLocal index)
  TrQuotation{} -> error "quotation appeared during conversion to IR"
  TrText x _ -> value $ IrString x
  where
  value :: IrValue -> K IrBlock
  value = return . V.singleton . IrPush

terminated :: IrBlock -> IrBlock
terminated = (<> V.singleton (IrReturn 0))

data FlattenedProgram = FlattenedProgram
  { flattenedBlock :: !IrBlock
  , flattenedNames :: !(DefIdMap Int)
  , flattenedSymbols :: !(DefIdMap [Name])
  }

declareBlockM :: Maybe Name -> IrBlock -> K DefId
declareBlockM name block = liftState $ state (declareBlock name block)

declareBlock :: Maybe Name -> IrBlock -> Program -> (DefId, Program)
declareBlock mSymbol block program@Program{..} = let
  (i, program') = freshDefId program
  in (,) i program'
    { programBlocks = Id.insert i block programBlocks
    , programSymbols = maybe id (`H.insert` i) mSymbol programSymbols
    }

getDefM :: Name -> K DefId
getDefM = liftState . state . getDef

getDef :: Name -> Program -> (DefId, Program)
getDef name program@Program{..} = case H.lookup name programSymbols of
  Just id' -> (id', program)
  Nothing -> let
    (id', program') = freshDefId program
    in (,) id' program'
      { programSymbols = H.insert name id' programSymbols }

inverseSymbols :: Program -> DefIdMap [Name]
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
  go (blocks, names) (name, block) =
    ( blocks <> V.singleton (IrLabel name) <> block
    , Id.insert name (V.length blocks) names
    )

  instructions :: DefIdMap IrBlock
  instructions = Id.adjust terminated entryId programBlocks
