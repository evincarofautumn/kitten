{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PostfixOperators #-}

module Kitten.Infer
  ( infer
  , typeFragment
  , forAll
  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Map ((!))

import qualified Data.Map as Map
import qualified Data.Set as Set

import Kitten.ClosedName
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Infer.Monad
import Kitten.Infer.Scheme
import Kitten.Infer.Type
import Kitten.Infer.Unify
import Kitten.Location
import Kitten.Name
import Kitten.Resolved
import Kitten.Type (Type((:&), (:.), (:?), (:|)))
import Kitten.Type hiding (Type(..))
import Kitten.Util.FailWriter
import Kitten.Util.Void

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Resolved as Resolved
import qualified Kitten.Type as Type

typeFragment
  :: Fragment Value Void
  -> Fragment Resolved.Value Resolved
  -> Either [CompileError] ()
typeFragment prelude fragment
  = fst . runInference emptyEnv
  $ inferFragment prelude fragment

inferFragment
  :: Fragment Value Void
  -> Fragment Value Resolved
  -> Inferred ()
inferFragment prelude fragment = do

  forM_ (zip [0..] allDefs) $ \ (index, def)
    -> case manifestType (defTerm def) of
      Just scheme -> saveDef index scheme
      Nothing -> case defAnno def of
        Just anno -> saveDef index =<< fromAnno anno
        Nothing -> Inferred . throwMany . (:[]) . TypeError (defLocation def)
          $ "missing type declaration for " ++ defName def

  forM_ (zip [(0 :: Int)..] allDefs) $ \ (index, def)
    -> withLocation (defLocation def) $ do
      scheme <- generalize (inferValue (defTerm def))
      declaredScheme <- getsEnv ((! Name index) . envDefs)
      declared <- instantiateM declaredScheme
      inferred <- instantiateM scheme
      declared === inferred

  Type.Function consumption _ _ <- infer
    $ Compose (fragmentTerms fragment) UnknownLocation

  consumption === Type.Empty

  where

  allDefs = fragmentDefs prelude ++ fragmentDefs fragment

  saveDef :: Int -> Scheme -> Inferred ()
  saveDef index scheme = modifyEnv $ \ env -> env
    { envDefs = Map.insert (Name index) scheme (envDefs env) }

class ForAll a b where
  forAll :: a -> Inferred (Type b)

instance ForAll a b => ForAll (Type c -> a) b where
  forAll f = do
    var <- freshVarM
    forAll $ f var

instance ForAll (Type a) a where
  forAll = pure

-- | Infers the type of a term.
infer :: Resolved -> Inferred (Type Scalar)
infer resolved = case resolved of

  Builtin name loc -> withLocation loc $ case name of

    Builtin.AddVector -> forAll $ \ r a
      -> r :. Type.Vector a :. Type.Vector a
      --> r :. Type.Vector a

    Builtin.AddFloat -> binary Type.Float

    Builtin.AddInt -> binary Type.Int

    Builtin.AndBool -> binary Type.Bool

    Builtin.AndInt -> binary Type.Int

    Builtin.Apply -> forAll $ \ r s
      -> r :. (r --> s) --> s

    Builtin.Call -> forAll $ \ r s
      -> r :. (r ==> s) ==> s

    Builtin.CharToInt -> forAll $ \ r
      -> r :. Type.Char --> r :. Type.Int

    Builtin.Close -> forAll $ \ r
      -> r :. Type.Handle ==> r

    Builtin.DivFloat -> binary Type.Float
    Builtin.DivInt -> binary Type.Int

    Builtin.EqFloat -> relational Type.Float
    Builtin.EqInt -> relational Type.Int

    Builtin.Exit -> forAll $ \ r
      -> r :. Type.Int ==> r

    Builtin.First -> forAll $ \ r a b
      -> r :. a :& b --> r :. a

    Builtin.FromLeft -> forAll $ \ r a b
      -> r :. a :| b --> r :. a

    Builtin.FromRight -> forAll $ \ r a b
      -> r :. a :| b --> r :. b

    Builtin.FromSome -> forAll $ \ r a
      -> r :. (a :?) --> r :. a

    Builtin.GeFloat -> relational Type.Float
    Builtin.GeInt -> relational Type.Int

    Builtin.Get -> forAll $ \ r a
      -> r :. Type.Vector a :. Type.Int --> r :. a

    Builtin.GetLine -> forAll $ \ r
      -> r :. Type.Handle ==> r :. string

    Builtin.GtFloat -> relational Type.Float
    Builtin.GtInt -> relational Type.Int

    Builtin.Impure -> forAll $ \ r
      -> r ==> r

    Builtin.Init -> forAll $ \ r a
      -> r :. Type.Vector a --> r :. Type.Vector a

    Builtin.IsNone -> forAll $ \ r a
      -> r :. (a :?) --> r :. Type.Bool

    Builtin.IsRight -> forAll $ \ r a b
      -> r :. a :| b --> r :. Type.Bool

    Builtin.LeFloat -> relational Type.Float
    Builtin.LeInt -> relational Type.Int

    Builtin.Left -> forAll $ \ r a b
      -> r :. a --> r :. a :| b

    Builtin.Length -> forAll $ \ r a
      -> r :. Type.Vector a --> r :. Type.Int

    Builtin.LtFloat -> relational Type.Float
    Builtin.LtInt -> relational Type.Int

    Builtin.ModFloat -> binary Type.Float
    Builtin.ModInt -> binary Type.Int

    Builtin.MulFloat -> binary Type.Float
    Builtin.MulInt -> binary Type.Int

    Builtin.NeFloat -> relational Type.Float
    Builtin.NeInt -> relational Type.Int

    Builtin.NegFloat -> unary Type.Float
    Builtin.NegInt -> unary Type.Int

    Builtin.None -> forAll $ \ r a
      -> r --> r :. (a :?)

    Builtin.NotBool -> unary Type.Bool
    Builtin.NotInt -> unary Type.Int

    Builtin.OpenIn -> forAll $ \ r
      -> r :. string ==> r :. Type.Handle

    Builtin.OpenOut -> forAll $ \ r
      -> r :. string ==> r :. Type.Handle

    Builtin.OrBool -> binary Type.Bool
    Builtin.OrInt -> binary Type.Int

    Builtin.Rest -> forAll $ \ r a b
      -> r :. a :& b --> r :. b

    Builtin.Right -> forAll $ \ r a b
      -> r :. b --> r :. a :| b

    Builtin.Set -> forAll $ \ r a
      -> r :. Type.Vector a :. a :. Type.Int --> r :. Type.Vector a

    Builtin.ShowFloat -> forAll $ \ r
      -> r :. Type.Float --> r :. string

    Builtin.ShowInt -> forAll $ \ r
      -> r :. Type.Int --> r :. string

    Builtin.Some -> forAll $ \ r a
      -> r :. a --> r :. (a :?)

    Builtin.Stderr -> forAll $ \ r
      -> r --> r :. Type.Handle
    Builtin.Stdin -> forAll $ \ r
      -> r --> r :. Type.Handle
    Builtin.Stdout -> forAll $ \ r
      -> r --> r :. Type.Handle

    Builtin.SubFloat -> binary Type.Float
    Builtin.SubInt -> binary Type.Int

    Builtin.Pair -> forAll $ \ r a b
      -> r :. a :. b --> r :. a :& b

    Builtin.Print -> forAll $ \ r
      -> r :. string :. Type.Handle ==> r

    Builtin.Tail -> forAll $ \ r a
      -> r :. Type.Vector a --> r :. Type.Vector a

    Builtin.UnsafePurify11 -> forAll $ \ r s a b
      -> r :. (s :. a ==> s :. b) --> r :. (s :. a --> s :. b)

    Builtin.Vector -> forAll $ \ r a
      -> r :. a --> r :. Type.Vector a

    Builtin.XorBool -> binary Type.Bool
    Builtin.XorInt -> binary Type.Int

  Call name loc -> withLocation loc
    $ instantiateM =<< getsEnv ((! name) . envDefs)

  Compose terms loc -> withLocation loc $ do
    types <- mapM infer terms
    r <- freshVarM
    foldM
      (\ (Type.Function a b e1) (Type.Function c d e2)
        -> inferCompose a b c d e1 e2)
      (r --> r)
      types

  Group terms loc -> infer (Compose terms loc)

  If true false loc -> withLocation loc $ do
    Type.Function a b e1 <- infer true
    Type.Function c d e2 <- infer false
    a === c
    b === d
    return $ Type.Function (a :. Type.Bool) b (e1 +: e2)

  PairTerm a b loc -> withLocation loc $ do
    a' <- fromConstant =<< infer a
    b' <- fromConstant =<< infer b
    forAll $ \ r -> r --> r :. a' :& b'

  Push value loc -> withLocation loc $ do
    a <- inferValue value
    forAll $ \ r -> r --> r :. a

  Scoped term loc -> withLocation loc $ do
    a <- freshVarM
    Type.Function b c p <- local a $ infer term
    return $ Type.Function (b :. a) c p

  VectorTerm values loc -> withLocation loc $ do
    values' <- mapM infer values
    values'' <- fromConstant =<< unifyEach values'
    forAll $ \ r -> r --> r :. Type.Vector values''

fromConstant :: Type Scalar -> Inferred (Type Scalar)
fromConstant type_ = do
  a <- freshVarM
  r <- freshVarM
  type_ === r --> r :. a
  return a

binary :: Type Scalar -> Inferred (Type Scalar)
binary a = forAll $ \ r -> r :. a :. a --> r :. a

relational :: Type Scalar -> Inferred (Type Scalar)
relational a = forAll $ \ r -> r :. a :. a --> r :. Type.Bool

unary :: Type Scalar -> Inferred (Type Scalar)
unary a = forAll $ \ r -> r :. a --> r :. a

string :: Type Scalar
string = Type.Vector Type.Char

local :: Type Scalar -> Inferred a -> Inferred a
local type_ action = do
  modifyEnv $ \ env -> env { envLocals = type_ : envLocals env }
  result <- action
  modifyEnv $ \ env -> env { envLocals = tail $ envLocals env }
  return result

withClosure :: [Type Scalar] -> Inferred a -> Inferred a
withClosure types action = do
  original <- getsEnv envClosure
  modifyEnv $ \ env -> env { envClosure = types }
  result <- action
  modifyEnv $ \ env -> env { envClosure = original }
  return result

getClosedName :: ClosedName -> Inferred (Type Scalar)
getClosedName name = case name of
  ClosedName (Name index) -> getsEnv $ (!! index) . envLocals
  ReclosedName (Name index) -> getsEnv $ (!! index) . envClosure

manifestType :: Value -> Maybe Scheme
manifestType value = case value of
  Activation{} -> Nothing
  Bool{} -> Just $ mono Type.Bool
  Char{} -> Just $ mono Type.Char
  Choice{} -> Nothing
  Closed{} -> Nothing
  Closure _ (Compose [Push constant _] _)
    -> manifestConstant constant
  Closure{} -> Nothing
  Float{} -> Just $ mono Type.Float
  Function (Compose [Push constant _] _)
    -> manifestConstant constant
  Function{} -> Nothing
  Handle{} -> Just $ mono Type.Handle
  Int{} -> Just $ mono Type.Int
  Local{} -> Nothing
  Option{} -> Nothing
  Pair a b -> do
    Forall rows1 scalars1 effects1 type1 <- manifestType a
    Forall rows2 scalars2 effects2 type2 <- manifestType b
    return $ Forall
      (rows1 <> rows2)
      (scalars1 <> scalars2)
      (effects1 <> effects2)
      (type1 :& type2)
  Unit -> Just $ mono Type.Unit
  Vector{} -> Nothing

  where
  manifestConstant constant = do
    Forall rows scalars effects type_ <- manifestType constant
    return $ Forall
      (Set.insert (row (Name 0)) rows)
      scalars
      effects
      (Type.Var (Name 0) --> Type.Var (Name 0) :. type_)

inferValue :: Value -> Inferred (Type Scalar)
inferValue value = case value of
  Activation{} -> error "TODO infer activation"
  Bool{} -> return Type.Bool
  Char{} -> return Type.Char
  Choice True a -> (:|) <$> freshVarM <*> inferValue a
  Choice False a -> (:|) <$> inferValue a <*> freshVarM
  Closed (Name index) -> getsEnv ((!! index) . envClosure)
  Closure names term -> do
    closed <- mapM getClosedName names
    withClosure closed (infer term)
  Float{} -> return Type.Float
  Function term -> infer term
  Handle{} -> return Type.Handle
  Int{} -> return Type.Int
  Local (Name index) -> getsEnv ((!! index) . envLocals)
  Option Nothing -> (:?) <$> freshVarM
  Option (Just a) -> (:?) <$> inferValue a
  Pair a b -> (:&) <$> inferValue a <*> inferValue b
  Unit -> return Type.Unit
  Vector values -> do
    valueTypes <- mapM inferValue values
    valueType <- unifyEach valueTypes
    return $ Type.Vector valueType

unifyEach :: [Type Scalar] -> Inferred (Type Scalar)
unifyEach (x : y : zs) = x === y >> unifyEach (y : zs)
unifyEach [x] = return x
unifyEach [] = freshVarM

inferCompose
  :: Type Row -> Type Row
  -> Type Row -> Type Row
  -> Type Effect -> Type Effect
  -> Inferred (Type Scalar)
inferCompose in1 out1 in2 out2 e1 e2 = do
  out1 === in2
  return $ Type.Function in1 out2 (e1 +: e2)
