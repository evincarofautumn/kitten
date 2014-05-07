{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Optimize
  ( optimize
  ) where

import Data.Function
import Data.List
import Data.Maybe
-- import Data.Monoid
-- import Data.Set (Set)

import qualified Data.Set as S
-- import qualified Data.Traversable as T
import qualified Data.Vector as V

import Kitten.IdMap (DefIdMap)
import Kitten.Types

import qualified Kitten.IdMap as Id

optimize :: Program -> Program
optimize program = program
  { programBlocks = whole $ fix go (programBlocks program) }
  where
  go = \loop blocks -> let
    blocks' = foldl' (applyOpt program) blocks optimizations
    in if blocks' == blocks
      then blocks
      else loop blocks'

whole :: DefIdMap IrBlock -> DefIdMap IrBlock
whole defs = Id.filterWithKey
  (\k _ -> k == entryId || (entryId, k) `elem` closure) defs
  where
  closure = transitiveClosure references
  references = concatMap (\ (a, b) -> map ((,) a) (S.toList b))
    $ Id.toList $ Id.map (S.fromList . mapMaybe reference . V.toList) defs
  reference = \case
    IrAct x _ _ -> Just x
    IrCall x -> Just x
    IrTailCall x -> Just x
    _ -> Nothing

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
  -> DefIdMap IrBlock
  -> (Program -> Optimization)
  -> DefIdMap IrBlock
applyOpt program blocks opt
  = Id.map (V.fromList . opt program . V.toList) blocks
