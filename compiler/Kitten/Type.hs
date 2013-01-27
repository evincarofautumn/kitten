{-# LANGUAGE OverloadedStrings #-}

module Kitten.Type
  ( Type(..)
  , Typed(..)
  , TypeScheme(..)
  , Value(..)
  , manifestType
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Vector as Vector

import Kitten.Builtin (Builtin)
import Kitten.Name

data Type
  = BoolType
  | IntType
  | TextType
  | !Type :> !Type
  | !Type :. !Type
  | SVec !Type !Int
  | DVec !Type
  | TupleType !(Vector Type)
  | EmptyType
  | Var !Name
  deriving (Eq, Ord)

infixl 4 :>
infixl 5 :.

data Typed
  = Value !Value
  | Builtin !Builtin !Type
  | Scoped !Typed !Type
  | Local !Name !Type
  | Compose !Typed !Typed !Type
  | Empty !Type

data Value
  = Word !Name !Type
  | Int !Int !Type
  | Bool !Bool !Type
  | Text !Text !Type
  | Vec !(Vector Value) !Type
  | Tuple !(Vector Value) !Type
  | Fun !Typed !Type

data TypeScheme = Forall [Name] Type

manifestType :: Typed -> Type
manifestType term = case term of
  Value value -> manifestValueType value
  Builtin _ type_ -> type_
  Scoped _ type_ -> type_
  Local _ type_ -> type_
  Compose _ _ type_ -> type_
  Empty type_ -> type_
  where

manifestValueType :: Value -> Type
manifestValueType value = case value of
  Word _ type_ -> type_
  Int _ type_ -> type_
  Bool _ type_ -> type_
  Text _ type_ -> type_
  Vec _ type_ -> type_
  Fun _ type_ -> type_
  Tuple _ type_ -> type_

instance Show Type where
  show IntType = "int"
  show BoolType = "bool"
  show TextType = "text"
  show (Var name) = show name
  show (SVec type_ size)
    = show type_ ++ "[" ++ show size ++ "]"
  show (DVec type_)
    = show type_ ++ "*"
  show (TupleType types)
    = "(" ++ unwords (map show $ Vector.toList types) ++ ")"
  show (a :> b)
    = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (EmptyType :. a) = show a
  show EmptyType = "()"
  show (a :. b) = show a ++ " " ++ show b

instance Show TypeScheme where
  show (Forall vars term)
    = "forall [" ++ unwords (map show vars) ++ "]. " ++ show term
