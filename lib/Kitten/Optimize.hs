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
    blocks' = foldl' applyOpt blocks [callElim, leaveElim]
    in if blocks' == blocks
      then blocks
      else loop blocks'

type Optimization = [IrInstruction] -> [IrInstruction]

applyOpt
  :: DefIdMap (Vector IrInstruction)
  -> Optimization
  -> DefIdMap (Vector IrInstruction)
applyOpt blocks opt = Id.map (V.fromList . opt . V.toList) blocks

callElim :: Optimization
callElim (IrCall x : IrReturn : xs) = IrTailCall x : callElim xs
callElim (x : xs) = x : callElim xs
callElim [] = []

leaveElim :: Optimization
leaveElim (IrLeave : IrReturn : xs) = IrReturn : leaveElim xs
leaveElim (x : xs) = x : leaveElim xs
leaveElim [] = []

