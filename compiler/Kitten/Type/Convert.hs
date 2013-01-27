module Kitten.Type.Convert
  ( toTyped
  , toTypedDef
  , toTypedFragment
  ) where

import Control.Applicative

import qualified Data.Vector as Vector

import Kitten.Def
import Kitten.Fragment
import Kitten.Resolve (Resolved)
import Kitten.Type
import Kitten.Type.Inference

import qualified Kitten.Resolve as Resolve

-- | Annotates a resolved AST with fresh type variables.
toTypedFragment :: Fragment Resolved -> Inference (Fragment Typed)
toTypedFragment (Fragment defs term)
  = Fragment <$> Vector.mapM toTypedDef defs <*> toTyped term

-- | Annotates a resolved definition with fresh type variables.
toTypedDef :: Def Resolved -> Inference (Def Typed)
toTypedDef (Def name term) = Def name <$> toTyped term

-- | Annotates a resolved term with fresh type variables.
toTyped :: Resolved -> Inference Typed
toTyped resolved = case resolved of
  Resolve.Value value -> Value <$> toTypedValue value
  Resolve.Builtin name -> Builtin name <$> freshFunction
  Resolve.Scoped term -> Scoped <$> toTyped term <*> freshFunction
  Resolve.Local index -> Local index <$> freshFunction
  Resolve.Compose down top
    -> Compose <$> toTyped down <*> toTyped top <*> freshFunction
  Resolve.Empty -> Empty <$> freshFunction
  where
  toTypedValue v = case v of
    Resolve.Word index -> Word index <$> freshFunction
    Resolve.Int value -> Int value <$> freshFunction
    Resolve.Bool value -> Bool value <$> freshFunction
    Resolve.Text value -> Text value <$> freshFunction
    Resolve.Vec terms -> Vec
      <$> Vector.mapM toTypedValue terms <*> freshFunction
    Resolve.Fun term -> Fun <$> toTyped term <*> freshFunction
    Resolve.Tuple terms -> Tuple
      <$> Vector.mapM toTypedValue terms <*> freshFunction
