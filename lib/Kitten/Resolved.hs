{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Kitten.Resolved
  ( Resolved(..)
  , Value(..)
  , charsFromString
  , stringFromChars
  ) where

import Data.Monoid
import Data.Vector (Vector)
import System.IO

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.AST
import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Location
import Kitten.Name
import Kitten.Util.Text (Text, ToText(..), showText)

data Resolved
  = Builtin !Builtin !Location
  | Call !Name !Location
  | Compose !(Vector Resolved) !Location
  | From !Text !Location
  | PairTerm !Resolved !Resolved !Location
  | Push !Value !Location
  | To !Text !Location
  | Scoped !Resolved !Location
  | VectorTerm !(Vector Resolved) !Location
  deriving (Eq, Show)

data Value
  = Activation !(Vector Value) !Resolved
  | Bool !Bool
  | Char !Char
  | Choice !Bool !Value
  | Closed !Name
  | Closure !(Vector ClosedName) !Resolved
  | Float !Double
  | Function !Resolved
  | Handle !Handle
  | Int !Int
  | Local !Name
  | Option !(Maybe Value)
  | Pair !Value !Value
  | Unit
  | Vector !(Vector Value)
  | Wrapped !Text !Value
  deriving (Eq)

instance AST Resolved where
  type TermValue Resolved = Value

instance Show Value where
  show = T.unpack . toText

instance ToText Value where
  toText value = case value of
    Activation{} -> "<function>"
    Bool b -> if b then "true" else "false"
    Char c -> showText c
    Choice which v -> T.unwords
      [toText v, if which then "right" else "left"]
    Closed{} -> "<closed>"
    Closure{} -> "<function>"
    Float f -> showText f
    Function{} -> "<function>"
    Handle{} -> "<handle>"
    Int i -> showText i
    Local{} -> "<local>"
    Option m -> maybe "none" ((<> " some") . toText) m
    Pair a b -> T.unwords [toText a, toText b, "pair"]
    Unit -> "()"
    Vector v@(V.toList -> (Char _ : _)) -> showText (stringFromChars v)
    Vector v -> T.concat
      [ "["
      , T.intercalate ", " (V.toList (V.map toText v))
      , "]"
      ]
    Wrapped name v -> T.unwords [toText v, "to", name]

stringFromChars :: Vector Value -> String
stringFromChars = V.toList . V.map fromChar
  where
  fromChar :: Value -> Char
  fromChar (Char c) = c
  fromChar _ = error "stringFromChars: non-character"

charsFromString :: String -> Vector Value
charsFromString = V.fromList . map Char
