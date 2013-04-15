module Kitten.Type
  ( Type(..)
  ) where

import Kitten.Name

data Type
  = BoolType
  | IntType
  | TextType
  | Type :> Type
  | Type :. Type
  | VecType Type
  | TupleType [Type]
  | Var Name
  | EmptyType
  deriving (Eq, Ord)

infix 4 :>
infixl 5 :.

instance Show Type where
  show IntType = "int"
  show BoolType = "bool"
  show TextType = "text"
  show (Var name) = show name
  show (VecType type_)
    = show type_ ++ "*"
  show (TupleType types)
    = "(" ++ unwords (map show types) ++ ")"
  show (a :> b)
    = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (EmptyType :. a) = show a
  show EmptyType = "()"
  show (a :. b) = show a ++ " " ++ show b
