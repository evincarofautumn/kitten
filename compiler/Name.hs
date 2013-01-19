module Name
  ( Name(..)
  ) where

newtype Name = Name Int
  deriving (Enum, Eq, Ord)

instance Show Name where
  show (Name name) = 't' : show name
