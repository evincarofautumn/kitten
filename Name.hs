module Name
  ( Name(..)
  ) where

newtype Name = Name Int
  deriving (Eq, Ord)

instance Show Name where
  show (Name name) = 't' : show name
