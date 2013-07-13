{-# LANGUAGE RecordWildCards #-}

module Kitten.Yarn
  ( Instruction(..)
  , Label
  , Value(..)
  , yarn
  ) where

import Control.Applicative
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
  | Enter
  | Jump Label
  | JumpIfFalse Offset
  | Leave
  | Label Label
  | Local Index
  | MakeVector Int
  | Push Value
  | Return

instance Show Instruction where
  show instruction = case instruction of
    Act label names -> unwords
      [ "act"
      , show label
      , concatMap showClosedName names
      ]
      where
      showClosedName :: ClosedName -> String
      showClosedName (ClosedName (Name index)) = "Local:" ++ show index
      showClosedName (ReclosedName (Name index)) = "Closure:" ++ show index

    Builtin builtin -> show builtin
    Call label -> unwords ["call", show label]
    Closure index -> unwords ["closure", show index]
    Enter -> "enter"
    Jump label -> unwords ["jmp", show label]
    JumpIfFalse offset -> unwords ["jf", show offset]
    Leave -> "leave"
    Label label -> unwords ["label", show label]
    Local index -> unwords ["local", show index]
    MakeVector size -> unwords ["vector", show size]
    Push value -> unwords ["push", show value]
    Return -> "ret"

data Value
  = Bool Bool
  | Char Char
  | Float Double
  | Handle Handle
  | Int Int
  | Vector [Value]
  | Word Label                 
  | Pair Value Value
  | Unit

instance Show Value where
  show value = case value of
    Bool bool -> unwords ["bool", if bool then "1" else "0"]
    Char char -> unwords ["char", show (fromEnum char :: Int)]
    Float float -> unwords ["float", show float]

    -- FIXME Unnecessary?
    Handle handle -> case () of
      _ | handle == stderr -> "handle 2"
        | handle == stdin -> "handle 0"
        | handle == stdout -> "handle 1"
        | otherwise -> "handle 0"

    Int int -> unwords ["int", show int]
    Vector values -> unwords
      $ "vector" : show (length values) : map show values
    Word label -> unwords ["word", show label]
    Pair a b -> unwords ["pair", show a, show b]
    Unit -> "unit 0"

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
    <*> concatMapM yarnTerm fragmentTerms

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
  return $ Label index : instructions ++ [Return]

yarnTerm :: Resolved -> Yarn [Instruction]
yarnTerm term = case term of
  Resolved.Call (Name index) _ -> return [Call index]
  Resolved.Compose terms -> concatMapM yarnTerm terms
  Resolved.Builtin builtin _ -> return [Builtin builtin]

  Resolved.If true false _ -> do
    true' <- yarnTerm true
    false' <- yarnTerm false
    return $ concat
      [ [JumpIfFalse $ length true']
      , true'
      , false'
      ]

  Resolved.PairTerm a b _ -> do
    a' <- yarnTerm a
    b' <- yarnTerm b
    return $ a' ++ b' ++ [Builtin Builtin.Pair]

  Resolved.Push value _ -> yarnValueInstruction value
  Resolved.Scoped terms _ -> do
    instructions <- yarnTerm terms
    return $ Enter : instructions ++ [Leave]

  Resolved.VectorTerm values _ -> do
    values' <- concatMapM yarnTerm values
    return $ values' ++ [MakeVector (length values)]

yarnValueInstruction
  :: Resolved.Value
  -> Yarn [Instruction]
yarnValueInstruction resolved = case resolved of
  Resolved.Activation{} -> error
    "Kitten.Yarn.yarnValueInstruction: unexpected activation"
  Resolved.Closed (Name index) {- _ -} -> return [Closure index]
  Resolved.Closure names terms -> do
    instructions <- yarnTerm terms
    index <- yarnClosure instructions
    return [Act index names]
  Resolved.Function{} -> error
    "Kitten.Yarn.yarnValueInstruction: unresolved closure"
  Resolved.Local (Name index) {- _ -} -> return [Local index]
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
  Resolved.Float value -> Float value
  Resolved.Handle value -> Handle value
  Resolved.Int value -> Int value
  Resolved.Pair a b -> Pair (yarnValue a) (yarnValue b)
  Resolved.Unit -> Unit
  Resolved.Vector values -> Vector (map yarnValue values)
  _ -> error "Kitten.Yarn.yarnValue: instruction where value expected"
