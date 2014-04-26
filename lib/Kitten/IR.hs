{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -w #-}

module Kitten.IR
  ( Block
  , FlattenedProgram(..)
  , Instruction(..)
  , Program(..)
  , Value(..)
  , declareBlock
  , emptyProgram
  , entryId
  , flattenProgram
  , ir
  ) where

import Control.Applicative hiding (some)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import System.IO

import qualified Data.Char as Char
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Id
import Kitten.IdMap (IdMap)
import Kitten.Tree (TypedTerm, TypedValue)
import Kitten.Type (Kind(..), Type)
import Kitten.Util.Monad
import Kitten.Util.Text (ToText(..), showText)
import Kitten.Util.Tuple

import qualified Kitten.Builtin as Builtin
import qualified Kitten.IdMap as Id
import qualified Kitten.Tree as Tree
import qualified Kitten.Type as Type

type Label = Int
type Offset = Int
type Index = Int

data Instruction
  = Act !Id !(Vector ClosedName) !(Type Scalar)
  | Builtin !Builtin
  | Call !Id
  | Closure !Index
  | Comment !Text
  | Enter
  | Leave
  | Local !Index
  | MakeVector !Int
  | Push !Value
  | Return

type Block = Vector Instruction

instance Show Instruction where
  show instruction = T.unpack . T.unwords $ case instruction of
    Act target names _
      -> "act" : showText target : map showClosedName (V.toList names)
      where
      showClosedName :: ClosedName -> Text
      showClosedName (ClosedName index) = "local:" <> showText index
      showClosedName (ReclosedName index) = "closure:" <> showText index

    Builtin builtin -> ["builtin", toText builtin]
    Call target -> ["call", showText target]
    Closure index -> ["closure", showText index]
    Comment comment -> ["\n;", comment]
    Enter -> ["enter"]
    Leave -> ["leave"]
    Local index -> ["local", showText index]
    MakeVector size -> ["vector", showText size]
    Push value -> ["push", showText value]
    Return -> ["ret"]

data Value
  = Bool !Bool
  | Char !Char
  | Choice !Bool !Value
  | Float !Double
  | Handle !Handle
  | Int !Int
  | Option !(Maybe Value)
  | Pair !Value !Value
  | String !Text

instance Show Value where
  show = T.unpack . toText

instance ToText Value where
  toText value = T.unwords $ case value of
    Bool bool -> ["bool", if bool then "1" else "0"]
    Char char -> ["char", showText (Char.ord char)]
    Choice which choice
      -> [if which then "right" else "left", toText choice]
    Float float -> ["float", showText float]

    -- FIXME Unnecessary?
    Handle handle -> (:[]) $ case () of
      _ | handle == stderr -> "handle 2"
        | handle == stdin -> "handle 0"
        | handle == stdout -> "handle 1"
        | otherwise -> "handle 0"

    Int int -> ["int", showText int]
    Option Nothing -> ["none"]
    Option (Just option) -> ["some", toText option]
    Pair a b -> ["pair", toText a, toText b]
    String string
      -> "vector"
      : showText (T.length string)
      : map (\char -> "char " <> showText (Char.ord char))
        (T.unpack string)

type IR a = State Program a

ir :: Fragment TypedTerm -> Program -> Program
ir Fragment{..} program = flip execState program $ do
  F.mapM_ irDef fragmentDefs
  irEntry fragmentTerms

irDef :: Def TypedTerm -> IR ()
irDef Def{..} = do
  defId <- getDefM defName
  block <- terminated <$> irTerm (Type.unScheme defTerm)
  modify $ \program@Program{..} -> program
    { programBlocks = Id.insert defId block programBlocks }

irEntry :: Vector TypedTerm -> IR ()
irEntry terms = do
  instructions <- concatMapM irTerm terms
  modify $ \program@Program{..} -> program
    { programBlocks = Id.adjust (<> instructions) entryId programBlocks }

irTerm :: TypedTerm -> IR Block
irTerm term = case term of
  Tree.Builtin builtin _ -> return $ V.singleton (Builtin builtin)
  Tree.Call _ target _ -> do
    target' <- getDefM target
    return $ V.singleton (Call target')
  Tree.Compose _ terms _ -> concatMapM irTerm terms
  Tree.Lambda _ terms _ -> do
    instructions <- irTerm terms
    return $ V.singleton Enter <> instructions <> V.singleton Leave
  Tree.PairTerm a b _ -> do
    a' <- irTerm a
    b' <- irTerm b
    return $ a' <> b' <> V.singleton (Builtin Builtin.Pair)
  Tree.Push value _ -> irValue value
  Tree.VectorTerm values _ -> do
    values' <- concatMapM irTerm values
    return $ values' <> (V.singleton . MakeVector $ V.length values)

irValue :: TypedValue -> IR Block
irValue resolved = case resolved of
  Tree.Bool x _ -> value $ Bool x
  Tree.Char x _ -> value $ Char x
  Tree.Closed index _ -> return $ V.singleton (Closure index)
  Tree.Closure names terms (loc, type_) -> do
    instructions <- irTerm terms
    target <- declareBlockM (Just $ "closure from " <> toText loc) (terminated instructions)
    return $ V.singleton (Act target names type_)
  Tree.Float x _ -> value $ Float x
  Tree.Int x _ -> value $ Int x
  Tree.Local index _ -> return $ V.singleton (Local index)
  Tree.String x _ -> value $ String x
  Tree.Quotation{} -> error "quotation appeared during conversion to IR"
  where
  value :: Value -> IR Block
  value = return . V.singleton . Push

terminated :: Block -> Block
terminated = (<> V.singleton Return)

data FlattenedProgram = FlattenedProgram
  { flattenedBlock :: !Block
  , flattenedNames :: !(IdMap Int)
  , flattenedSymbols :: !(IdMap [Text])
  }

declareBlockM :: Maybe Text -> Block -> IR Id
declareBlockM = (state .) . declareBlock

declareBlock :: Maybe Text -> Block -> Program -> (Id, Program)
declareBlock mSymbol block program@Program{..} = let
  (i, idGen') = genId programIdGen
  in (,) i program
    { programBlocks = Id.insert i block programBlocks
    , programIdGen = idGen'
    , programSymbols = maybe id (`H.insert` i) mSymbol programSymbols
    }

getDefM :: Text -> IR Id
getDefM = state . getDef

getDef :: Text -> Program -> (Id, Program)
getDef name program@Program{..} = case H.lookup name programSymbols of
  Just id' -> (id', program)
  Nothing -> let
    (id', idGen') = genId programIdGen
    in (,) id' program
      { programIdGen = idGen'
      , programSymbols = H.insert name id' programSymbols
      }

flattenProgram :: Program -> FlattenedProgram
flattenProgram Program{..} = FlattenedProgram{..}
  where
  (flattenedBlock, flattenedNames) = foldl' go mempty (Id.toList instructions)
  flattenedSymbols
    = foldl' (\symbols (symbol, name)
      -> Id.insertWith (++) name [symbol] symbols) Id.empty
    $ H.toList programSymbols

  go :: (Block, IdMap Int) -> (Id, Block) -> (Block, IdMap Int)
  go (blocks, names) (name, block)
    = (blocks <> block, Id.insert name (V.length blocks) names)

  instructions :: IdMap Block
  instructions = Id.adjust terminated entryId programBlocks
