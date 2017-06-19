{-|
Module      : Kitten.Stack
Description : Strict stack
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Kitten.Stack
  ( Stack(..)
  , empty
  , fromList
  , pop
  , pop'
  , pops
  , pushes
  ) where

-- | A stack with strictly evaluated elements and spine.
data Stack a = Bottom | !a ::: !(Stack a)
  deriving (Functor, Foldable)

infixr 5 :::

empty :: Stack a -> Bool
empty Bottom = True
empty _ = False

fromList :: [a] -> Stack a
fromList = foldr (:::) Bottom

pop :: Stack a -> Maybe (a, Stack a)
pop Bottom = Nothing
pop (a ::: s) = Just (a, s)

pop' :: Stack a -> Stack a
pop' Bottom = error "Kitten.Stack.drop: empty stack"
pop' (_ ::: s) = s

pushes :: [a] -> Stack a -> Stack a
pushes xs s = foldr (:::) s xs

pops :: Int -> Stack a -> ([a], Stack a)
pops n s
  | n <= 0 = ([], s)
  | otherwise = case s of
    Bottom -> ([], s)
    a ::: s' -> let
      (as, s'') = pops (n - 1) s'
      in (a : as, s'')
