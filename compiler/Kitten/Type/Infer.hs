{-# LANGUAGE OverloadedStrings #-}

module Kitten.Type.Infer
  ( typeFragment
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Monoid
import Data.Vector (Vector, (!))

import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Kitten.Anno (Anno(..), Sig)
import Kitten.Def
import Kitten.Error
import Kitten.Name
import Kitten.Fragment
import Kitten.Resolve (Resolved)
import Kitten.Type
import Kitten.Type.Convert
import Kitten.Type.Inference
import Kitten.Type.Scheme
import Kitten.Type.Substitute
import Kitten.Type.Unify

import qualified Kitten.Anno as Anno
import qualified Kitten.Builtin as Builtin
import qualified Kitten.Resolve as Resolve

-- | Runs type inference on a resolved AST.
typeFragment
  :: Vector (Def Resolved)
  -> Vector (Anno Name)
  -> [Resolve.Value]
  -> Fragment Resolved Name
  -> Either CompileError (Fragment Typed Name)
typeFragment prelude annos stack fragment
  = fmap (uncurry substFragment) . flip runStateT emptyEnv $ do
    typedPrelude <- Vector.mapM toTypedDef prelude
    typedFragment <- toTypedFragment fragment
    inferFragment stack typedPrelude annos typedFragment

-- | Infers and annotates the type of a program fragment.
inferFragment
  :: [Resolve.Value]
  -> Vector (Def Typed)
  -> Vector (Anno Name)
  -> Fragment Typed Name
  -> Inference (Fragment Typed Name)
inferFragment stack prelude annos fragment@Fragment{..}
  = inferTerm >> inferAllDefs >> gets (substFragment fragment)
  where
  inferAllDefs = do
    inferred <- (<>) <$> inferDefs prelude <*> inferDefs fragmentDefs
    instantiations <- gets envInstantiations
    forM_ instantiations $ \ (type_, Name index)
      -> unifyM_ type_ =<< instantiate (inferred ! index)
    let allAnnos = fragmentAnnos <> annos
    Vector.forM_ allAnnos $ \ (Anno (Name index) vars sig) -> do
      declaredType <- instantiate . Forall vars =<< typeFromSig sig
      inferredType <- instantiate $ inferred ! index
      unifyM_ declaredType inferredType
  inferTerm = do
    (a :> b) <- infer <=< toTyped $ stackTerm stack
    (c :> _) <- infer fragmentTerm
    s <- fresh
    unifyM_ a (EmptyType :> s)
    unifyM_ b c
  inferDefs = Vector.mapM inferDef
  stackTerm = Resolve.Compose
    . Vector.map Resolve.Value . Vector.reverse . Vector.fromList

typeFromSig :: Sig -> Inference Type
typeFromSig s = case s of
  Anno.Function left right
    -> (:>) <$> typeFromSig left <*> typeFromSig right
  Anno.Compose down top -> (:.) <$> typeFromSig down <*> typeFromSig top
  Anno.Vec sig -> VecType <$> typeFromSig sig
  Anno.Tuple sigs -> TupleType <$> Vector.mapM typeFromSig sigs
  Anno.Empty -> return EmptyType
  Anno.Var name -> return $ Var name
  Anno.Word "bool" -> return BoolType
  Anno.Word "int" -> return IntType
  Anno.Word "text" -> return TextType
  Anno.Word name -> fail . Text.unpack $ Text.concat
    [ "Unable to resolve type '"
    , name
    , "'"
    ]

-- | Infers the type scheme of a definition.
inferDef :: Def Typed -> Inference TypeScheme
inferDef (Def _ term) = generalize $ infer term

-- | Infers the type of a term.
infer :: Typed -> Inference Type
infer typedTerm = do
  r <- fresh
  let
    boolToBool  = return $ r :. BoolType             :> r :. BoolType
    boolsToBool = return $ r :. BoolType :. BoolType :> r :. BoolType
    intToInt    = return $ r :. IntType              :> r :. IntType
    intsToBool  = return $ r :. IntType :. IntType   :> r :. BoolType
    intsToInt   = return $ r :. IntType :. IntType   :> r :. IntType

  case typedTerm of
    Value value -> case value of
      Word name type_ -> unifyM type_ =<< makeInstantiation name
      Int _ type_ -> unifyM type_ $ r :> r :. IntType
      Bool _ type_ -> unifyM type_ $ r :> r :. BoolType
      Text _ type_ -> unifyM type_ $ r :> r :. TextType
      Vec terms type_ -> do
        termTypes <- mapM (infer . Value) $ Vector.toList terms
        termType <- unifyEach termTypes
        unifyM type_ $ r :> r :. VecType termType
        where
        unifyEach (x:y:zs) = unifyM x y >> unifyEach (y:zs)
        unifyEach [x] = return x
        unifyEach [] = fresh
      Tuple terms type_ -> do
        termTypes <- Vector.mapM (infer . Value) terms
        unifyM type_ $ r :> r :. TupleType termTypes
      Fun x type_ -> do
        a <- infer x
        unifyM type_ $ r :> r :. a
    -- Note the similarity to composition here.
    Scoped term type_ -> do
      a <- fresh
      local (Forall mempty a) $ do
        (b :> c) <- infer term
        unifyM_ r b
        unifyM type_ $ r :. a :> c
    Local (Name index) type_ -> do
      termType <- instantiate =<< gets ((!! index) . envLocals)
      unifyM type_ $ r :> r :. termType
    Compose x y type_ -> do
      (a :> b) <- infer x
      (c :> d) <- infer y
      unifyM_ b c
      unifyM type_ $ a :> d
    Empty type_ -> unifyM type_ $ r :> r
    Builtin name type_ -> unifyM type_ =<< case name of
      Builtin.Print -> return $ r :. TextType :> r
      Builtin.At
        -> (\ a -> r :. VecType a :. IntType :> r :. a)
        <$> fresh
      Builtin.Cat
        -> (\ a -> r :. VecType a :. VecType a :> r :. VecType a)
        <$> fresh
      Builtin.Vec
        -> (\ a -> r :. a :> r :. VecType a)
        <$> fresh
      Builtin.Top
        -> (\ a -> r :. VecType a :> r :. a)
        <$> fresh
      Builtin.Down
        -> (\ a -> r :. VecType a :> r :. VecType a)
        <$> fresh
      Builtin.Bottom
        -> (\ a -> r :. VecType a :> r :. a)
        <$> fresh
      Builtin.Up
        -> (\ a -> r :. VecType a :> r :. VecType a)
        <$> fresh
      Builtin.Length
        -> (\ a -> r :. VecType a :> r :. IntType)
        <$> fresh
      Builtin.Empty
        -> (\ a -> r :. VecType a :> r :. BoolType)
        <$> fresh
      Builtin.Dup
        -> (\ a -> r :. a :> r :. a :. a)
        <$> fresh
      Builtin.Swap
        -> (\ a b -> r :. a :. b :> r :. b :. a)
        <$> fresh <*> fresh
      Builtin.Drop
        -> (\ a -> r :. a :> r)
        <$> fresh
      Builtin.Fun
        -> (\ s a -> r :. a :> r :. (s :> s :. a))
        <$> fresh <*> fresh
      Builtin.Apply
        -> (\ s -> r :. (r :> s) :> s)
        <$> fresh
      Builtin.Compose
        -> (\ a b c -> r :. (a :> b) :. (b :> c) :> r :. (a :> c))
        <$> fresh <*> fresh <*> fresh
      Builtin.If
        -> (\ s -> r :. (r :> s) :. (r :> s) :. BoolType :> s)
        <$> fresh
      Builtin.AndBool -> boolsToBool
      Builtin.AndInt  -> intsToInt
      Builtin.OrBool  -> boolsToBool
      Builtin.OrInt   -> intsToInt
      Builtin.XorBool -> boolsToBool
      Builtin.XorInt  -> intsToInt
      Builtin.NotBool -> boolToBool
      Builtin.NotInt  -> intToInt
      Builtin.Add     -> intsToInt
      Builtin.Sub     -> intsToInt
      Builtin.Mul     -> intsToInt
      Builtin.Div     -> intsToInt
      Builtin.Mod     -> intsToInt
      Builtin.Neg     -> intToInt
      Builtin.Eq      -> intsToBool
      Builtin.Ne      -> intsToBool
      Builtin.Lt      -> intsToBool
      Builtin.Gt      -> intsToBool
      Builtin.Le      -> intsToBool
      Builtin.Ge      -> intsToBool
  where
  local scheme action = do
    modify $ \ env -> env { envLocals = scheme : envLocals env }
    result <- action
    modify $ \ env -> env { envLocals = tail $ envLocals env }
    return result
