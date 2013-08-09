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
import System.IO

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Name
import Kitten.Resolved (Resolved)
import Kitten.Util.Monad

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Resolved as Resolved

type Label = Int
type Offset = Int
type Index = Int

data Instruction
  = Act Label [ClosedName]
  | Builtin Builtin
  | Call Label
  | Closure Index
  | Comment String
  | Enter
  | EntryLabel
  | Jump Offset
  | JumpIfFalse Offset
  | JumpIfNone Offset
  | JumpIfRight Offset
  | Leave
  | Label Label
  | Local Index
  | MakeVector Int
  | Push Value
  | Return

instance Show Instruction where
  show instruction = unwords $ case instruction of
    Act label names
      -> "act" : show label : map showClosedName names
      where
      showClosedName :: ClosedName -> String
      showClosedName (ClosedName (Name index)) = "local:" ++ show index
      showClosedName (ReclosedName (Name index)) = "closure:" ++ show index

    Builtin builtin -> ["builtin", show builtin]
    Call label -> ["call", show label]
    Closure index -> ["closure", show index]
    Comment comment -> ["\n;", comment]
    Enter -> ["enter"]
    EntryLabel -> ["\nentry"]
    Jump offset -> ["jmp", show offset]
    JumpIfFalse offset -> ["jf", show offset]
    JumpIfNone offset -> ["jn", show offset]
    JumpIfRight offset -> ["jr", show offset]
    Leave -> ["leave"]
    Label label -> ["\nlabel", show label]
    Local index -> ["local", show index]
    MakeVector size -> ["vector", show size]
    Push value -> ["push", show value]
    Return -> ["ret"]

data Value
  = Bool Bool
  | Char Char
  | Choice Bool Value
  | Float Double
  | Handle Handle
  | Int Int
  | Option (Maybe Value)
  | Pair Value Value
  | Unit
  | Vector [Value]
  | Word Label                 

instance Show Value where
  show value = unwords $ case value of
    Bool bool -> ["bool", if bool then "1" else "0"]
    Char char -> ["char", show (fromEnum char :: Int)]
    Choice which choice
      -> [if which then "right" else "left", show choice]
    Float float -> ["float", show float]

    -- FIXME Unnecessary?
    Handle handle -> (:[]) $ case () of
      _ | handle == stderr -> "handle 2"
        | handle == stdin -> "handle 0"
        | handle == stdout -> "handle 1"
        | otherwise -> "handle 0"

    Int int -> ["int", show int]
    Option Nothing -> ["none"]
    Option (Just option) -> ["some", show option]
    Pair a b -> ["pair", show a, show b]
    Vector values
      -> "vector" : show (length values) : map show values
    Word label -> ["word", show label]
    Unit -> ["unit"]

data Env = Env
  { envClosures :: [[Instruction]]
  }

type Yarn a = ReaderT Int (State Env) a

yarn
  :: Fragment Resolved.Value Resolved
  -> [Instruction]
yarn Fragment{..}
  = collectClosures . withClosureOffset $ (++)
    <$> concatMapM (uncurry yarnDef) (zip fragmentDefs [0..])
    <*> yarnEntry fragmentTerms

  where
  closureOffset :: Int
  closureOffset = length fragmentDefs

  withClosureOffset
    :: Yarn a
    -> State Env a
  withClosureOffset = flip runReaderT closureOffset

  collectClosures
    :: State Env [Instruction]
    -> [Instruction]
  collectClosures action = let
    (instructions, Env{..}) = runState action Env { envClosures = [] }
    in concatMap (uncurry collectClosure)
        (zip [closureOffset..] envClosures)
      ++ instructions

  collectClosure
    :: Int
    -> [Instruction]
    -> [Instruction]
  collectClosure index instructions
    = Label index : instructions ++ [Return]

yarnDef
  :: Def Resolved.Value
  -> Int
  -> Yarn [Instruction]
yarnDef Def{..} index = do
  instructions <- case defTerm of
    Resolved.Closure [] term -> yarnTerm term
    _ -> error "Kitten.Yarn.yarnDef: TODO yarn non-function definition"
  return
    $ Comment defName
    : Label index
    : instructions
    ++ [Return]

yarnEntry :: [Resolved] -> Yarn [Instruction]
yarnEntry terms = do
  instructions <- concatMapM yarnTerm terms
  return $ EntryLabel : instructions ++ [Return]

yarnTerm :: Resolved -> Yarn [Instruction]
yarnTerm term = case term of
  Resolved.Builtin builtin _ -> return [Builtin builtin]
  Resolved.Call (Name index) _ -> return [Call index]
  Resolved.ChoiceTerm left right _ -> do
    left' <- yarnTerm left
    right' <- yarnTerm right
    return $ concat
      [ [JumpIfRight $ length left' + 1]
      , left'
      , [Jump $ length right']
      , right'
      ]
  Resolved.Compose terms _ -> concatMapM yarnTerm terms
  Resolved.From{} -> return []
  Resolved.Group terms _ -> concatMapM yarnTerm terms
  Resolved.If true false _ -> do
    true' <- yarnTerm true
    false' <- yarnTerm false
    return $ concat
      [ [JumpIfFalse $ length true' + 1]
      , true'
      , [Jump $ length false']
      , false'
      ]
  Resolved.OptionTerm some none _ -> do
    some' <- yarnTerm some
    none' <- yarnTerm none
    return $ concat
      [ [JumpIfNone $ length some' + 1]
      , some'
      , [Jump $ length none']
      , none'
      ]
  Resolved.PairTerm a b _ -> do
    a' <- yarnTerm a
    b' <- yarnTerm b
    return $ a' ++ b' ++ [Builtin Builtin.Pair]
  Resolved.Push value _ -> yarnValueInstruction value
  Resolved.Scoped terms _ -> do
    instructions <- yarnTerm terms
    return $ Enter : instructions ++ [Leave]
  Resolved.To{} -> return []
  Resolved.VectorTerm values _ -> do
    values' <- concatMapM yarnTerm values
    return $ values' ++ [MakeVector (length values)]

yarnValueInstruction
  :: Resolved.Value
  -> Yarn [Instruction]
yarnValueInstruction resolved = case resolved of
  Resolved.Activation{} -> error
    "Kitten.Yarn.yarnValueInstruction: unexpected activation"
  Resolved.Closed (Name index) -> return [Closure index]
  Resolved.Closure names terms -> do
    instructions <- yarnTerm terms
    index <- yarnClosure instructions
    return [Act index names]
  Resolved.Function{} -> error
    "Kitten.Yarn.yarnValueInstruction: unresolved closure"
  Resolved.Local (Name index) -> return [Local index]
  _ -> return [Push $ yarnValue resolved]

yarnClosure :: [Instruction] -> Yarn Label
yarnClosure terms = do
  closureOffset <- ask
  label <- lift . gets $ length . envClosures
  lift . modify $ \ env@Env{..} -> env
    { envClosures = envClosures ++ [terms] }
  return $ label + closureOffset

yarnValue :: Resolved.Value -> Value
yarnValue resolved = case resolved of
  Resolved.Bool value -> Bool value
  Resolved.Char value -> Char value
  Resolved.Choice which value -> Choice which (yarnValue value)
  Resolved.Float value -> Float value
  Resolved.Handle value -> Handle value
  Resolved.Int value -> Int value
  Resolved.Option value -> Option (yarnValue <$> value)
  Resolved.Pair a b -> Pair (yarnValue a) (yarnValue b)
  Resolved.Unit -> Unit
  Resolved.Vector values -> Vector (map yarnValue values)
  Resolved.Activation{} -> unexpectedInstruction
  Resolved.Closed{} -> unexpectedInstruction
  Resolved.Closure{} -> unexpectedInstruction
  Resolved.Function{} -> unexpectedInstruction
  Resolved.Local{} -> unexpectedInstruction
  where
  unexpectedInstruction = error
    "Kitten.Yarn.yarnValue: instruction where value expected"
