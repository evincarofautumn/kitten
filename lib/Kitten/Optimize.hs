{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Optimize
  ( optimize
  ) where

import Data.Function
import Data.List

import qualified Data.Set as S
import qualified Data.Vector as V

import Kitten.IdMap (DefIdMap)
import Kitten.Types
import Kitten.Util.Function
import Kitten.Util.Maybe

import qualified Kitten.IdMap as Id

optimize :: OptConfig -> Program -> Program
optimize config program = program
  { programBlocks = whole config $ fix go (programBlocks program) }
  where
  go = \loop blocks -> let
    blocks' = foldl' (applyOpt program) blocks optimizations
    in if blocks' == blocks
      then blocks
      else loop blocks'

whole :: OptConfig -> DefIdMap IrBlock -> DefIdMap IrBlock
whole OptConfig{..} = compose [unusedDefElim | optUnusedDefElim]

unusedDefElim :: DefIdMap IrBlock -> DefIdMap IrBlock
unusedDefElim defs = Id.filterWithKey
  (\k _ -> k == entryId || (entryId, k) `elem` closure) defs
  where
  closure = transitiveClosure references
  references = concatMap (\ (a, b) -> map ((,) a) (S.toList b))
    $ Id.toList $ Id.map (S.fromList . concatMap reference . V.toList) defs
  reference = \case
    IrAct x _ _ -> [x]
    IrCall x -> [x]
    IrTailCall _ x -> [x]
    IrMatch cases mDefault -> mDefault
      `consMaybe` V.toList (V.map (\ (IrCase _ x) -> x) cases)
    _ -> []

-- TODO Make more efficient.
transitiveClosure :: (Eq a) => [(a, a)] -> [(a, a)]
transitiveClosure xs = if xs == acc then xs else transitiveClosure acc
  where acc = nub $ xs ++ [(a, d) | (a, b) <- xs, (c, d) <- xs, b == c]

type Optimization = [IrInstruction] -> [IrInstruction]

optimizations :: [Program -> Optimization]
optimizations =
  [ callElim
  , inline
  , leaveElim
  ]

callElim :: a -> Optimization
callElim p (IrCall x : IrReturn locals : xs)
  = IrTailCall locals x : callElim p xs
callElim p (x : xs) = x : callElim p xs
callElim _ [] = []

inline :: Program -> Optimization
inline p (call@(IrCall x) : xs) = ($ inline p xs)
  $ case Id.lookup x (programBlocks p) of
    Just block
      | V.length block < inlineThreshold
      , V.length block >= 1
      , IrReturn n <- V.last block
      -> ((V.toList (V.init block) ++ [IrLeave n]) ++)
    _ -> (call :)
inline p (x : xs) = x : inline p xs
inline _ [] = []

inlineThreshold :: Int
inlineThreshold = 10

leaveElim :: a -> Optimization
leaveElim _ = go
  where
  go (IrLeave m : IrLeave n : xs) = IrLeave (m + n) : go xs
  go (IrLeave m : IrReturn n : xs) = IrReturn (m + n) : go xs
  go (x : xs) = x : go xs
  go [] = []

applyOpt
  :: Program
  -> DefIdMap IrBlock
  -> (Program -> Optimization)
  -> DefIdMap IrBlock
applyOpt program blocks opt
  = Id.map (V.fromList . opt program . V.toList) blocks
