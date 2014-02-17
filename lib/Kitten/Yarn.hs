{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Yarn
  ( Instruction(..)
  , Label
  , Value(..)
  , yarn
  ) where

import Control.Applicative hiding (some)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import System.IO

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Name
import Kitten.Tree (TypedTerm, TypedValue)
import Kitten.Util.Monad
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Type as Type
import qualified Kitten.Tree as Tree

type Label = Int
type Offset = Int
type Index = Int

data Instruction
  = Act !Label !(Vector ClosedName)
  | Builtin !Builtin
  | Call !Label
  | Closure !Index
  | Comment !Text
  | Enter
  | EntryLabel
  | Jump !Offset
  | JumpIfFalse !Offset
  | JumpIfNone !Offset
  | JumpIfRight !Offset
  | Leave
  | Label !Label
  | Local !Index
  | MakeVector !Int
  | Push !Value
  | Return

instance Show Instruction where
  show instruction = T.unpack . T.unwords $ case instruction of
    Act label names
      -> "act" : showText label : map showClosedName (V.toList names)
      where
      showClosedName :: ClosedName -> Text
      showClosedName (ClosedName (Name index)) = "local:" <> showText index
      showClosedName (ReclosedName (Name index)) = "closure:" <> showText index

    Builtin builtin -> ["builtin", toText builtin]
    Call label -> ["call", showText label]
    Closure index -> ["closure", showText index]
    Comment comment -> ["\n;", comment]
    Enter -> ["enter"]
    EntryLabel -> ["\nentry"]
    Jump offset -> ["jmp", showText offset]
    JumpIfFalse offset -> ["jf", showText offset]
    JumpIfNone offset -> ["jn", showText offset]
    JumpIfRight offset -> ["jr", showText offset]
    Leave -> ["leave"]
    Label label -> ["\nlabel", showText label]
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
  | Unit
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
    Unit -> ["unit"]

data Env = Env
  { envClosures :: [Vector Instruction]
  }

type Yarn a = ReaderT Int (State Env) a

yarn
  :: Fragment TypedTerm
  -> Vector Instruction
yarn Fragment{..}
  = collectClosures . withClosureOffset $ (<>)
    <$> yarnEntry fragmentTerms
    <*> concatMapM (uncurry yarnDef)
      (V.zip fragmentDefs (V.fromList [0..V.length fragmentDefs]))

  where
  closureOffset :: Int
  closureOffset = V.length fragmentDefs

  withClosureOffset
    :: Yarn a
    -> State Env a
  withClosureOffset = flip runReaderT closureOffset

  collectClosures
    :: State Env (Vector Instruction)
    -> Vector Instruction
  collectClosures action = let
    (instructions, Env{..}) = runState action Env { envClosures = [] }
    in instructions <> V.concatMap (uncurry collectClosure)
      (V.fromList (zip [closureOffset..] (reverse envClosures)))

  collectClosure
    :: Int
    -> Vector Instruction
    -> Vector Instruction
  collectClosure index instructions
    = V.singleton (Label index) <> instructions <> V.singleton Return

yarnDef
  :: Def TypedTerm
  -> Int
  -> Yarn (Vector Instruction)
yarnDef Def{..} index = do
  instructions <- yarnTerm (Type.unScheme defTerm)
  return
    $ V.fromList [Comment defName, Label index]
    <> instructions
    <> V.singleton Return

yarnEntry :: Vector TypedTerm -> Yarn (Vector Instruction)
yarnEntry terms = do
  instructions <- concatMapM yarnTerm terms
  return
    $ V.singleton EntryLabel
    <> instructions
    <> V.singleton Return

yarnTerm :: TypedTerm -> Yarn (Vector Instruction)
yarnTerm term = case term of
  Tree.Builtin builtin _ -> return $ V.singleton (Builtin builtin)
  Tree.Call _ (Name index) _ -> return $ V.singleton (Call index)
  Tree.Compose _ terms _ -> concatMapM yarnTerm terms
  Tree.Lambda _ terms _ -> do
    instructions <- yarnTerm terms
    return $ V.singleton Enter <> instructions <> V.singleton Leave
  Tree.PairTerm a b _ -> do
    a' <- yarnTerm a
    b' <- yarnTerm b
    return $ a' <> b' <> V.singleton (Builtin Builtin.Pair)
  Tree.Push value _ -> yarnValue value
  Tree.VectorTerm values _ -> do
    values' <- concatMapM yarnTerm values
    return $ values' <> (V.singleton . MakeVector $ V.length values)

yarnValue
  :: TypedValue
  -> Yarn (Vector Instruction)
yarnValue resolved = case resolved of
  Tree.Bool x _ -> value $ Bool x
  Tree.Char x _ -> value $ Char x
  Tree.Closed (Name index) _ -> return $ V.singleton (Closure index)
  Tree.Closure names terms _ -> do
    instructions <- yarnTerm terms
    index <- yarnClosure instructions
    return $ V.singleton (Act index names)
  Tree.Float x _ -> value $ Float x
  Tree.Int x _ -> value $ Int x
  Tree.Local (Name index) _ -> return $ V.singleton (Local index)
  Tree.String x _ -> value $ String x
  Tree.Quotation{} -> error "quotation appeared during conversion to IR"
  where
  value :: Value -> Yarn (Vector Instruction)
  value = return . V.singleton . Push

yarnClosure :: Vector Instruction -> Yarn Label
yarnClosure terms = do
  closureOffset <- ask
  label <- lift . gets $ length . envClosures
  lift . modify $ \env@Env{..} -> env
    { envClosures = terms : envClosures }
  return $ label + closureOffset
