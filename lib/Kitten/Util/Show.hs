{-# LANGUAGE ExistentialQuantification #-}

module Kitten.Util.Show
  ( Showable(..)
  , showWords
  ) where

data Showable = forall a. Show a => Showable a

instance Show Showable where
  show (Showable a) = show a

showWords :: (Show a) => [a] -> String
showWords = unwords . map show
