{-# LANGUAGE RecordWildCards #-}

module Kitten.Optimize
  ( optimize
  ) where

import Data.Function
import Data.List
import Data.Vector (Vector)

import qualified Data.Vector as V

import Kitten.IdMap (DefIdMap)
import Kitten.Types

import qualified Kitten.IdMap as Id

optimize :: Program -> Program
optimize program = program { programBlocks = fix go (programBlocks program) }
  where
  go = \loop blocks -> let
    blocks' = foldl' (applyOpt program) blocks optimizations
    in if blocks' == blocks
      then blocks
      else loop blocks'

type Optimization = [IrInstruction] -> [IrInstruction]

optimizations :: [Program -> Optimization]
optimizations =
  [ callElim
  , inline
  , leaveElim
  ]

callElim :: a -> Optimization
callElim p (IrCall x : IrReturn : xs) = IrTailCall x : callElim p xs
callElim p (x : xs) = x : callElim p xs
callElim _ [] = []

inline :: Program -> Optimization
inline p (IrCall x : xs) = ($ inline p xs)
  $ case Id.lookup x (programBlocks p) of
    Just block
      | V.length block < inlineThreshold
        && V.length block >= 1
        && V.last block == IrReturn
      -> (V.toList (V.init block) ++)
    _ -> (IrCall x :)
inline p (x : xs) = x : inline p xs
inline _ [] = []

inlineThreshold :: Int
inlineThreshold = 10

leaveElim :: a -> Optimization
leaveElim p (IrLeave : IrReturn : xs) = IrReturn : leaveElim p xs
leaveElim p (x : xs) = x : leaveElim p xs
leaveElim _ [] = []

applyOpt
  :: Program
  -> DefIdMap (Vector IrInstruction)
  -> (Program -> Optimization)
  -> DefIdMap (Vector IrInstruction)
applyOpt program blocks opt
  = Id.map (V.fromList . opt program . V.toList) blocks
