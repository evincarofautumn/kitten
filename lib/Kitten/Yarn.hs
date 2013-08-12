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

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Name
import Kitten.Resolved (Resolved)
import Kitten.Util.Monad
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Resolved as Resolved

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

    Builtin builtin -> ["builtin", showText builtin]
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
  | Vector !(Vector Value)
  | Word !Label                 

instance Show Value where
  show = T.unpack . toText

instance ToText Value where
  toText value = T.unwords $ case value of
    Bool bool -> ["bool", if bool then "1" else "0"]
    Char char -> ["char", showText (fromEnum char :: Int)]
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
    Vector values
      -> "vector"
      : showText (V.length values)
      : map toText (V.toList values)
    Word label -> ["word", showText label]
    Unit -> ["unit"]

data Env = Env
  { envClosures :: [Vector Instruction]
  }

type Yarn a = ReaderT Int (State Env) a

yarn
  :: Fragment Resolved.Value Resolved
  -> Vector Instruction
yarn Fragment{..}
  = collectClosures . withClosureOffset $ (<>)
    <$> concatMapM (uncurry yarnDef)
      (V.zip fragmentDefs (V.fromList [0..V.length fragmentDefs]))
    <*> yarnEntry fragmentTerms

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
    in V.concatMap (uncurry collectClosure)
        (V.fromList (zip [closureOffset..] envClosures))
      <> instructions

  collectClosure
    :: Int
    -> Vector Instruction
    -> Vector Instruction
  collectClosure index instructions
    = V.singleton (Label index) <> instructions <> V.singleton Return

yarnDef
  :: Def Resolved.Value
  -> Int
  -> Yarn (Vector Instruction)
yarnDef Def{..} index = do
  instructions <- case defTerm of
    Resolved.Closure _ term -> yarnTerm term
    _ -> error "Kitten.Yarn.yarnDef: TODO yarn non-function definition"
  return
    $ V.fromList [Comment defName, Label index]
    <> instructions
    <> V.singleton Return

yarnEntry :: Vector Resolved -> Yarn (Vector Instruction)
yarnEntry terms = do
  instructions <- concatMapM yarnTerm terms
  return
    $ V.singleton EntryLabel
    <> instructions
    <> V.singleton Return

yarnTerm :: Resolved -> Yarn (Vector Instruction)
yarnTerm term = case term of
  Resolved.Builtin builtin _ -> return $ V.singleton (Builtin builtin)
  Resolved.Call (Name index) _ -> return $ V.singleton (Call index)
  Resolved.ChoiceTerm left right _ -> do
    left' <- yarnTerm left
    right' <- yarnTerm right
    return $ V.concat
      [ V.singleton . JumpIfRight $ V.length left' + 1
      , left'
      , V.singleton . Jump $ V.length right'
      , right'
      ]
  Resolved.Compose terms _ -> concatMapM yarnTerm terms
  Resolved.From{} -> return V.empty
  Resolved.Group terms _ -> concatMapM yarnTerm terms
  Resolved.If true false _ -> do
    true' <- yarnTerm true
    false' <- yarnTerm false
    return $ V.concat
      [ V.singleton . JumpIfFalse $ V.length true' + 1
      , true'
      , V.singleton . Jump $ V.length false'
      , false'
      ]
  Resolved.OptionTerm some none _ -> do
    some' <- yarnTerm some
    none' <- yarnTerm none
    return $ V.concat
      [ V.singleton . JumpIfNone $ V.length some' + 1
      , some'
      , V.singleton . Jump $ V.length none'
      , none'
      ]
  Resolved.PairTerm a b _ -> do
    a' <- yarnTerm a
    b' <- yarnTerm b
    return $ a' <> b' <> V.singleton (Builtin Builtin.Pair)
  Resolved.Push value _ -> yarnValueInstruction value
  Resolved.Scoped terms _ -> do
    instructions <- yarnTerm terms
    return $ V.singleton Enter <> instructions <> V.singleton Leave
  Resolved.To{} -> return V.empty
  Resolved.VectorTerm values _ -> do
    values' <- concatMapM yarnTerm values
    return $ values' <> (V.singleton . MakeVector $ V.length values)

yarnValueInstruction
  :: Resolved.Value
  -> Yarn (Vector Instruction)
yarnValueInstruction resolved = case resolved of
  Resolved.Activation{} -> error
    "Kitten.Yarn.yarnValueInstruction: unexpected activation"
  Resolved.Closed (Name index) -> return $ V.singleton (Closure index)
  Resolved.Closure names terms -> do
    instructions <- yarnTerm terms
    index <- yarnClosure instructions
    return $ V.singleton (Act index names)
  Resolved.Function{} -> error
    "Kitten.Yarn.yarnValueInstruction: unresolved closure"
  Resolved.Local (Name index) -> return $ V.singleton (Local index)
  _ -> return $ V.singleton (Push $ yarnValue resolved)

yarnClosure :: Vector Instruction -> Yarn Label
yarnClosure terms = do
  closureOffset <- ask
  label <- lift . gets $ length . envClosures
  lift . modify $ \ env@Env{..} -> env
    { envClosures = terms : envClosures }
  return $ label + closureOffset

yarnValue :: Resolved.Value -> Value
yarnValue resolved = case resolved of
  Resolved.Activation{} -> unexpectedInstruction
  Resolved.Bool value -> Bool value
  Resolved.Char value -> Char value
  Resolved.Choice which value -> Choice which (yarnValue value)
  Resolved.Closed{} -> unexpectedInstruction
  Resolved.Closure{} -> unexpectedInstruction
  Resolved.Float value -> Float value
  Resolved.Function{} -> unexpectedInstruction
  Resolved.Handle value -> Handle value
  Resolved.Int value -> Int value
  Resolved.Local{} -> unexpectedInstruction
  Resolved.Option value -> Option (yarnValue <$> value)
  Resolved.Pair a b -> Pair (yarnValue a) (yarnValue b)
  Resolved.Unit -> Unit
  Resolved.Vector values -> Vector (yarnValue <$> values)
  Resolved.Wrapped _ inner -> yarnValue inner
  where
  unexpectedInstruction = error
    "Kitten.Yarn.yarnValue: instruction where value expected"
