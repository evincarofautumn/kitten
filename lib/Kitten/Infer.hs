{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Kitten.Infer
  ( infer
  , typeFragment
  , forAll
  ) where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Function
import Data.Monoid
import Data.Vector (Vector)

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

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
import Kitten.TypeDef
import Kitten.Util.FailWriter
import Kitten.Util.Text (toText)

import qualified Kitten.Builtin as Builtin
import qualified Kitten.NameMap as N
import qualified Kitten.Resolved as Resolved
import qualified Kitten.Type as Type

typeFragment
  :: [Value]
  -> Fragment Value Resolved
  -> Fragment Resolved.Value Resolved
  -> Either [CompileError] ()
typeFragment stack prelude fragment
  = fst . runInference emptyEnv
  $ inferFragment prelude fragment
    { fragmentTerms
      = V.map (`Push` UnknownLocation) (V.reverse (V.fromList stack))
      <> fragmentTerms fragment
    }

inferFragment
  :: Fragment Value Resolved
  -> Fragment Value Resolved
  -> Inferred ()
inferFragment prelude fragment = do

  F.forM_ allTypeDefs $ \ typeDef -> do
    let name = typeDefName typeDef
    scheme <- fromAnno (typeDefAnno typeDef)
    mExisting <- getsEnv (M.lookup name . envTypeDefs)
    case mExisting of
      Nothing -> modifyEnv $ \ env -> env
        { envTypeDefs = M.insert name scheme (envTypeDefs env) }
      Just existing
        -> Inferred . throwMany . (:[])
        . CompileError (typeDefLocation typeDef) $ T.unwords
          [ "multiple definitions of type"
          , name
          , "as both"
          , toText existing
          , "and"
          , toText scheme
          ]

  forM_ (zip [0..] (V.toList allDefs)) $ \ (index, def)
    -> case manifestType (defTerm def) of
      Just scheme -> saveDef index scheme
      Nothing -> case defAnno def of
        Just anno -> saveDef index =<< fromAnno anno
        Nothing -> do
          loc <- getsEnv envLocation
          saveDef index . mono
            =<< forAll (\ r s e -> Type.Function r s e loc)

  forM_ (zip [(0 :: Int)..] (V.toList allDefs)) $ \ (index, def)
    -> withLocation (defLocation def) $ do
      scheme <- generalize (inferValue (defTerm def))
      declaredScheme <- getsEnv ((N.! Name index) . envDefs)
      declared <- instantiateM declaredScheme
      inferred <- instantiateM scheme
      declared === inferred

  Type.Function consumption _ _ _ <- infer
    $ Compose (fragmentTerms fragment) UnknownLocation

  consumption === Type.Empty UnknownLocation

  where

  allTypeDefs = ((<>) `on` fragmentTypeDefs) prelude fragment
  allDefs = ((<>) `on` fragmentDefs) prelude fragment

  saveDef :: Int -> Scheme -> Inferred ()
  saveDef index scheme = modifyEnv $ \ env -> env
    { envDefs = N.insert (Name index) scheme (envDefs env) }

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
      -> r :. Type.Vector a loc :. Type.Vector a loc
      --> r :. Type.Vector a loc

    Builtin.AddFloat -> binary (Type.Float loc)

    Builtin.AddInt -> binary (Type.Int loc)

    Builtin.AndBool -> binary (Type.Bool loc)

    Builtin.AndInt -> binary (Type.Int loc)

    Builtin.Apply -> forAll $ \ r s e
      -> Type.Function (r :. Type.Function r s e loc) s e loc

    Builtin.CharToInt -> forAll $ \ r
      -> r :. Type.Char loc --> r :. Type.Int loc

    Builtin.Close -> forAll $ \ r
      -> r :. Type.Handle loc ==> r

    Builtin.DivFloat -> binary (Type.Float loc)
    Builtin.DivInt -> binary (Type.Int loc)

    Builtin.EqFloat -> relational (Type.Float loc) loc
    Builtin.EqInt -> relational (Type.Int loc) loc

    Builtin.Exit -> forAll $ \ r
      -> r :. Type.Int loc ==> r

    Builtin.First -> forAll $ \ r a b
      -> r :. a :& b --> r :. a

    Builtin.FromLeft -> forAll $ \ r a b
      -> r :. a :| b --> r :. a

    Builtin.FromRight -> forAll $ \ r a b
      -> r :. a :| b --> r :. b

    Builtin.FromSome -> forAll $ \ r a
      -> r :. (a :?) --> r :. a

    Builtin.GeFloat -> relational (Type.Float loc) loc
    Builtin.GeInt -> relational (Type.Int loc) loc

    Builtin.Get -> forAll $ \ r a
      -> r :. Type.Vector a loc :. Type.Int loc --> r :. a

    Builtin.GetLine -> forAll $ \ r
      -> r :. Type.Handle loc ==> r :. string loc

    Builtin.GtFloat -> relational (Type.Float loc) loc
    Builtin.GtInt -> relational (Type.Int loc) loc

    Builtin.Impure -> forAll $ \ r
      -> r ==> r

    Builtin.Init -> forAll $ \ r a
      -> r :. Type.Vector a loc --> r :. Type.Vector a loc

    Builtin.LeFloat -> relational (Type.Float loc) loc
    Builtin.LeInt -> relational (Type.Int loc) loc

    Builtin.Left -> forAll $ \ r a b
      -> r :. a --> r :. a :| b

    Builtin.Length -> forAll $ \ r a
      -> r :. Type.Vector a loc --> r :. Type.Int loc

    Builtin.LtFloat -> relational (Type.Float loc) loc
    Builtin.LtInt -> relational (Type.Int loc) loc

    Builtin.ModFloat -> binary (Type.Float loc)
    Builtin.ModInt -> binary (Type.Int loc)

    Builtin.MulFloat -> binary (Type.Float loc)
    Builtin.MulInt -> binary (Type.Int loc)

    Builtin.NeFloat -> relational (Type.Float loc) loc
    Builtin.NeInt -> relational (Type.Int loc) loc

    Builtin.NegFloat -> unary (Type.Float loc)
    Builtin.NegInt -> unary (Type.Int loc)

    Builtin.None -> forAll $ \ r a
      -> r --> r :. (a :?)

    Builtin.NotBool -> unary (Type.Bool loc)
    Builtin.NotInt -> unary (Type.Int loc)

    Builtin.OpenIn -> forAll $ \ r
      -> r :. string loc ==> r :. Type.Handle loc

    Builtin.OpenOut -> forAll $ \ r
      -> r :. string loc ==> r :. Type.Handle loc

    Builtin.OrBool -> binary (Type.Bool loc)
    Builtin.OrInt -> binary (Type.Int loc)

    Builtin.Rest -> forAll $ \ r a b
      -> r :. a :& b --> r :. b

    Builtin.Right -> forAll $ \ r a b
      -> r :. b --> r :. a :| b

    Builtin.Set -> forAll $ \ r a
      -> r :. Type.Vector a loc :. a :. Type.Int loc
      --> r :. Type.Vector a loc

    Builtin.ShowFloat -> forAll $ \ r
      -> r :. Type.Float loc --> r :. string loc

    Builtin.ShowInt -> forAll $ \ r
      -> r :. Type.Int loc --> r :. string loc

    Builtin.Some -> forAll $ \ r a
      -> r :. a --> r :. (a :?)

    Builtin.Stderr -> forAll $ \ r
      -> r --> r :. Type.Handle loc
    Builtin.Stdin -> forAll $ \ r
      -> r --> r :. Type.Handle loc
    Builtin.Stdout -> forAll $ \ r
      -> r --> r :. Type.Handle loc

    Builtin.SubFloat -> binary (Type.Float loc)
    Builtin.SubInt -> binary (Type.Int loc)

    Builtin.Pair -> forAll $ \ r a b
      -> r :. a :. b --> r :. a :& b

    Builtin.Print -> forAll $ \ r
      -> r :. string loc :. Type.Handle loc ==> r

    Builtin.Tail -> forAll $ \ r a
      -> r :. Type.Vector a loc --> r :. Type.Vector a loc

    Builtin.UnsafePurify11 -> forAll $ \ r s a b
      -> r :. (s :. a ==> s :. b) --> r :. (s :. a --> s :. b)

    Builtin.XorBool -> binary (Type.Bool loc)
    Builtin.XorInt -> binary (Type.Int loc)

  Call name loc -> withLocation loc
    $ instantiateM =<< getsEnv ((N.! name) . envDefs)

  ChoiceTerm left right loc -> withLocation loc $ do
    Type.Function a b e1 _ <- infer left
    Type.Function c d e2 _ <- infer right
    r <- freshVarM
    x <- freshVarM
    y <- freshVarM
    a === r :. x
    c === r :. y
    b === d
    return $ Type.Function (r :. x :| y) b (e1 +: e2) loc

  Compose terms loc -> withLocation loc $ do
    types <- V.mapM infer terms
    r <- freshVarM
    foldM
      (\ (Type.Function a b e1 _) (Type.Function c d e2 _)
        -> inferCompose a b c d e1 e2)
      (r --> r)
      (V.toList types)

  From name loc -> withLocation loc $ do
    underlying <- instantiateM =<< getsEnv ((M.! name) . envTypeDefs)
    forAll $ \r -> r :. Type.Named name loc --> r :. underlying

  Group terms loc -> infer (Compose terms loc)

  If true false loc -> withLocation loc $ do
    Type.Function a b e1 _ <- infer true
    Type.Function c d e2 _ <- infer false
    a === c
    b === d
    return $ Type.Function (a :. Type.Bool loc) b (e1 +: e2) loc

  OptionTerm some none loc -> withLocation loc $ do
    Type.Function a b e1 _ <- infer some
    Type.Function c d e2 _ <- infer none
    x <- freshVarM
    r <- freshVarM
    a === r :. x
    r === c
    b === d
    return $ Type.Function (r :. (x :?)) b (e1 +: e2) loc

  PairTerm a b loc -> withLocation loc $ do
    a' <- fromConstant =<< infer a
    b' <- fromConstant =<< infer b
    forAll $ \ r -> r --> r :. a' :& b'

  Push value loc -> withLocation loc $ do
    a <- inferValue value
    forAll $ \ r -> r --> r :. a

  Scoped term loc -> withLocation loc $ do
    a <- freshVarM
    Type.Function b c e _ <- local a $ infer term
    return $ Type.Function (b :. a) c e loc

  To name loc -> withLocation loc $ do
    underlying <- instantiateM =<< getsEnv ((M.! name) . envTypeDefs)
    forAll $ \r -> r :. underlying --> r :. Type.Named name loc

  VectorTerm values loc -> withLocation loc $ do
    values' <- mapM infer (V.toList values)
    values'' <- fromConstant =<< unifyEach values'
    forAll $ \ r -> r --> r :. Type.Vector values'' loc

fromConstant :: Type Scalar -> Inferred (Type Scalar)
fromConstant type_ = do
  a <- freshVarM
  r <- freshVarM
  type_ === r --> r :. a
  return a

binary :: Type Scalar -> Inferred (Type Scalar)
binary a = forAll $ \ r -> r :. a :. a --> r :. a

relational :: Type Scalar -> Location -> Inferred (Type Scalar)
relational a loc = forAll $ \ r -> r :. a :. a --> r :. Type.Bool loc

unary :: Type Scalar -> Inferred (Type Scalar)
unary a = forAll $ \ r -> r :. a --> r :. a

string :: Location -> Type Scalar
string loc = Type.Vector (Type.Char loc) loc

local :: Type Scalar -> Inferred a -> Inferred a
local type_ action = do
  modifyEnv $ \ env -> env { envLocals = type_ : envLocals env }
  result <- action
  modifyEnv $ \ env -> env { envLocals = tail $ envLocals env }
  return result

withClosure :: Vector (Type Scalar) -> Inferred a -> Inferred a
withClosure types action = do
  original <- getsEnv envClosure
  modifyEnv $ \ env -> env { envClosure = types }
  result <- action
  modifyEnv $ \ env -> env { envClosure = original }
  return result

getClosedName :: ClosedName -> Inferred (Type Scalar)
getClosedName name = case name of
  ClosedName (Name index) -> getsEnv $ (!! index) . envLocals
  ReclosedName (Name index) -> getsEnv $ (V.! index) . envClosure

manifestType :: Value -> Maybe Scheme
manifestType value = case value of
  Activation{} -> Nothing
  Bool{} -> Just $ mono (Type.Bool UnknownLocation)
  Char{} -> Just $ mono (Type.Char UnknownLocation)
  Choice{} -> Nothing
  Closed{} -> Nothing
  Closure _ (Compose (V.toList -> [Push constant _]) _)
    -> manifestConstant constant
  Closure{} -> Nothing
  Float{} -> Just $ mono (Type.Float UnknownLocation)
  Function (Compose (V.toList -> [Push constant _]) _)
    -> manifestConstant constant
  Function{} -> Nothing
  Handle{} -> Just $ mono (Type.Handle UnknownLocation)
  Int{} -> Just $ mono (Type.Int UnknownLocation)
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
  Unit -> Just $ mono (Type.Unit UnknownLocation)
  Vector{} -> Nothing
  Wrapped name _ -> Just $ mono (Type.Named name UnknownLocation)

  where
  manifestConstant constant = do
    Forall rows scalars effects type_ <- manifestType constant
    return $ Forall
      (S.insert (row (Name 0)) rows)
      scalars
      effects
      (Type.Var (Name 0) UnknownLocation
        --> Type.Var (Name 0) UnknownLocation :. type_)

inferValue :: Value -> Inferred (Type Scalar)
inferValue value = getsEnv envLocation >>= \ loc -> case value of
  Activation values term -> do
    closed <- V.mapM inferValue values
    withClosure closed (infer term)
  Bool{} -> return (Type.Bool loc)
  Char{} -> return (Type.Char loc)
  Choice True a -> (:|) <$> freshVarM <*> inferValue a
  Choice False a -> (:|) <$> inferValue a <*> freshVarM
  Closed (Name index) -> getsEnv ((V.! index) . envClosure)
  Closure names term -> do
    closed <- V.mapM getClosedName names
    withClosure closed (infer term)
  Float{} -> return (Type.Float loc)
  Function term -> infer term
  Handle{} -> return (Type.Handle loc)
  Int{} -> return (Type.Int loc)
  Local (Name index) -> getsEnv ((!! index) . envLocals)
  Option Nothing -> (:?) <$> freshVarM
  Option (Just a) -> (:?) <$> inferValue a
  Pair a b -> (:&) <$> inferValue a <*> inferValue b
  Unit -> return (Type.Unit loc)
  Vector values -> do
    valueTypes <- mapM inferValue (V.toList values)
    valueType <- unifyEach valueTypes
    return $ Type.Vector valueType loc
  Wrapped name _ -> return (Type.Named name loc)

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
  Type.Function in1 out2 (e1 +: e2) <$> getsEnv envLocation
