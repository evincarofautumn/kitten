{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kitten.Name
  ( Name(..)
  , nameIndex
  ) where

newtype Name = Name Int
  deriving (Enum, Eq, Ord)

instance Show Name where
  show (Name name) = '_' : show name

nameIndex :: Name -> Int
nameIndex (Name index) = index
