module Kitten.Type
  ( Type(..)
  , instanceOf
  ) where

import Kitten.Name

data Type
  = BoolType
  | IntType
  | TextType
  | Type :> Type
  | Composition [Type]
  | VecType Type
  | TupleType [Type]
  | Var Name
  | EmptyType
  deriving (Eq, Ord)

infix 4 :>

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
  show (Composition as) = unwords (map show as)
  show EmptyType = "()"

instanceOf :: Type -> Type -> Bool
instanceOf type1 type2
  | type1 == type2 = True
  | otherwise = case (type1, type2) of
    (Var a, Var b) -> a == b
    (_, Var _) -> True
    (a :> b, c :> d) -> instanceOf a c && instanceOf b d
    (Composition as, Composition bs) -> instancesOf as bs
    (VecType a, VecType b) -> a `instanceOf` b
    (TupleType as, TupleType bs) -> instancesOf as bs
    _ -> False
  where
  instancesOf :: [Type] -> [Type] -> Bool
  instancesOf as bs
    = length as == length bs
    && all (uncurry instanceOf) (zip as bs)
