{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RecursiveDo #-}

module Kitten.Infer
  ( infer
  , typeFragment
  , forAll
  ) where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Monoid
import Data.Vector (Vector)

import qualified Data.Foldable as F
import qualified Data.Map as M
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
import Kitten.Type hiding (Type(..), Local)
import Kitten.Typed (Typed)
import Kitten.TypeDef
import Kitten.Util.FailWriter
import Kitten.Util.Monad
import Kitten.Util.Text (toText)

import qualified Kitten.Builtin as Builtin
import qualified Kitten.NameMap as N
import qualified Kitten.Type as Type
import qualified Kitten.Typed as Typed
import qualified Kitten.Util.Vector as V

typeFragment
  :: Config
  -> Vector (Type Scalar)
  -> Fragment Typed
  -> Fragment Resolved
  -> NameGen
  -> Either [ErrorGroup] (NameGen, Fragment Typed, Type Scalar)
typeFragment config stackTypes prelude fragment nameGen
  = case run of
    (Left err, _) -> Left err
    (Right (typed, type_), env') -> Right (envNameGen env', typed, type_)
  where
  run :: (Either [ErrorGroup] (Fragment Typed, Type Scalar), Env)
  run = runInference config env
    $ inferFragment prelude fragment stackTypes
  env :: Env
  env = emptyEnv { envNameGen = nameGen }

inferFragment
  :: Fragment Typed
  -> Fragment Resolved
  -> Vector (Type Scalar)
  -> Inferred (Fragment Typed, Type Scalar)
inferFragment prelude fragment stackTypes = mdo

  -- Aggregate type definitions ('type Foo Bar').
  F.forM_ allTypeDefs $ \typeDef -> do
    let name = typeDefName typeDef
    scheme <- fromAnno (AnnotatedDef name) (typeDefAnno typeDef)
    mExisting <- getsEnv (M.lookup name . envTypeDefs)
    case mExisting of
      Nothing -> modifyEnv $ \env -> env
        { envTypeDefs = M.insert name scheme (envTypeDefs env) }
      Just existing
        -> liftFailWriter . throwMany . (:[]) . oneError
        . CompileError (typeDefLocation typeDef) Error $ T.unwords
          [ "multiple definitions of type"
          , name
          , "as both"
          , toText existing
          , "and"
          , toText scheme
          ]

  -- Populate environment with Prelude definition types.
  -- FIXME(strager): Use previously-inferred types (i.e.
  -- Typed.defTypeScheme def).  We cannot right now because
  -- effects are not inferred properly (e.g. for the effects
  -- of the 'map' prelude function).
  _ <- iforPreludeDefs save
  _ <- iforFragmentDefs save

  typedDefs <- iforFragmentDefs $ \index def
    -> withOrigin (defOrigin def) $ do
      (typedTerm, type_) <- infer finalEnv (defTerm def)
      inferredScheme <- generalize type_
      declaredScheme <- do
        decls <- getsEnv envDecls
        case N.lookup (Name index) decls of
          Just decl -> return decl
          Nothing -> do
            origin <- getsEnv envOrigin
            fmap mono . forAll $ \r s -> Type.Function r s origin
      saveDefWith (flip const) index inferredScheme

      (rowConsts, scalarConsts, declared) <- skolemize declaredScheme
      inferred <- instantiateM inferredScheme
      declared === inferred
      let
        (escapedRows, escapedScalars)
          = free declaredScheme <> free inferredScheme
        badRows = filter (`elem` escapedRows) rowConsts
        badScalars = filter (`elem` escapedScalars) scalarConsts
      unless (null badRows && null badScalars)
        $ error "SOMETHING SOMETHING BLARNEY"

      return def { defTerm = typedTerm <$ inferredScheme }

  (typedTerms, fragmentType) <- infer finalEnv
    $ Compose (fragmentTerms fragment) UnknownLocation

  -- Equate the bottom of the stack with stackTypes.
  do
    let Type.Function consumption _ _ = fragmentType
    bottom <- freshVarM
    enforce <- asksConfig enforceBottom
    when enforce $ bottom === Type.Empty (Origin NoHint UnknownLocation)
    let stackType = F.foldl (:.) bottom stackTypes
    stackType === consumption

  let
    typedFragment = Fragment
      { fragmentDefs = typedDefs
      , fragmentImports = fragmentImports fragment
      , fragmentTerms = V.singleton typedTerms
      , fragmentTypeDefs = fragmentTypeDefs fragment
      }

  finalEnv <- getEnv
  return (typedFragment, sub finalEnv fragmentType)

  where
  allTypeDefs = fragmentTypeDefs prelude <> fragmentTypeDefs fragment

  defOrigin :: Def a -> Origin
  defOrigin def = Origin
    (AnnoType (AnnotatedDef (defName def)))
    (defLocation def)

  preludeIndices, fragmentIndices :: [Int]
  (preludeIndices, fragmentIndices) = splitAt
    (V.length (fragmentDefs prelude)) [0..]

  iforFragmentDefs
    :: (Int -> Def Resolved -> Inferred a)
    -> Inferred (Vector a)
  iforFragmentDefs f = liftM V.fromList $ zipWithM f
    fragmentIndices
    (V.toList (fragmentDefs fragment))

  iforPreludeDefs
    :: (Int -> Typed.TypedDef -> Inferred a)
    -> Inferred (Vector a)
  iforPreludeDefs f = liftM V.fromList $ zipWithM f
    preludeIndices
    (V.toList (fragmentDefs prelude))

  saveDecl :: Int -> TypeScheme -> Inferred ()
  saveDecl index scheme = modifyEnv $ \env -> env
    { envDecls = N.insert (Name index) scheme (envDecls env) }

  saveDefWith
    :: (TypeScheme -> TypeScheme -> TypeScheme)
    -> Int
    -> TypeScheme
    -> Inferred ()
  saveDefWith f index scheme = modifyEnv $ \env -> env
    { envDefs = N.insertWith f
      (Name index) scheme (envDefs env) }

  save :: Int -> Def a -> Inferred ()
  save index def = case defAnno def of
    Just anno -> do
      scheme <- fromAnno annotated anno
      saveDecl index scheme
      saveDefWith const index scheme
    Nothing -> do
      scheme <- fmap mono . forAll $ \r s
        -> Type.Function r s (Origin (AnnoType annotated) (defLocation def))
      saveDecl index scheme
    where
    annotated :: Annotated
    annotated = AnnotatedDef (defName def)

class ForAll a b where
  forAll :: a -> Inferred (Type b)

instance ForAll a b => ForAll (Type c -> a) b where
  forAll f = do
    var <- freshVarM
    forAll $ f var

instance ForAll (Type a) a where
  forAll = pure

-- | Infers the type of a term.
infer :: Env -> Resolved -> Inferred (Typed, Type Scalar)
infer finalEnv resolved = case resolved of

  Kitten.Resolved.Builtin name loc -> asTyped (Typed.Builtin name) loc $ case name of

    Builtin.AddVector -> forAll $ \r a
      -> (r :. Type.Vector a o :. Type.Vector a o
      --> r :. Type.Vector a o) o

    Builtin.AddFloat -> binary (Type.Float o) o

    Builtin.AddInt -> binary (Type.Int o) o

    Builtin.AndBool -> binary (Type.Bool o) o

    Builtin.AndInt -> binary (Type.Int o) o

    Builtin.Apply -> forAll $ \r s
      -> Type.Function (r :. Type.Function r s o) s o

    Builtin.CharToInt -> forAll $ \r
      -> (r :. Type.Char o --> r :. Type.Int o) o

    Builtin.Choice -> forAll $ \r a b -> Type.Function
      (r :. (a :| b) :. Type.Function (r :. a) r o) r o

    Builtin.ChoiceElse -> forAll $ \r a b s -> Type.Function
      (r :. (a :| b)
        :. Type.Function (r :. a) s o
        :. Type.Function (r :. b) s o)
      s o

    Builtin.Close -> forAll $ \r
      -> (r :. Type.Handle o --> r) o

    Builtin.DivFloat -> binary (Type.Float o) o
    Builtin.DivInt -> binary (Type.Int o) o

    Builtin.EqFloat -> relational (Type.Float o) o
    Builtin.EqInt -> relational (Type.Int o) o

    Builtin.Exit -> forAll $ \r
      -> (r :. Type.Int o --> r) o

    Builtin.First -> forAll $ \r a b
      -> (r :. a :& b --> r :. a) o

    Builtin.FromLeft -> forAll $ \r a b
      -> (r :. a :| b --> r :. a) o

    Builtin.FromRight -> forAll $ \r a b
      -> (r :. a :| b --> r :. b) o

    Builtin.FromSome -> forAll $ \r a
      -> (r :. (a :?) --> r :. a) o

    Builtin.GeFloat -> relational (Type.Float o) o
    Builtin.GeInt -> relational (Type.Int o) o

    Builtin.Get -> forAll $ \r a
      -> (r :. Type.Vector a o :. Type.Int o --> r :. (a :?)) o

    Builtin.GetLine -> forAll $ \r
      -> (r :. Type.Handle o --> r :. string o) o

    Builtin.GtFloat -> relational (Type.Float o) o
    Builtin.GtInt -> relational (Type.Int o) o

    Builtin.If -> forAll $ \r -> Type.Function
      (r :. Type.Bool o :. Type.Function r r o) r o

    Builtin.IfElse -> forAll $ \r s -> Type.Function
      (r :. Type.Bool o
        :. Type.Function r s o
        :. Type.Function r s o)
      s o

    Builtin.Impure -> forAll $ \r
      -> (r --> r) o

    Builtin.Init -> forAll $ \r a
      -> (r :. Type.Vector a o --> r :. Type.Vector a o) o

    Builtin.IntToChar -> forAll $ \r
      -> (r :. Type.Int o --> r :. (Type.Char o :?)) o

    Builtin.LeFloat -> relational (Type.Float o) o
    Builtin.LeInt -> relational (Type.Int o) o

    Builtin.Left -> forAll $ \r a b
      -> (r :. a --> r :. a :| b) o

    Builtin.Length -> forAll $ \r a
      -> (r :. Type.Vector a o --> r :. Type.Int o) o

    Builtin.LtFloat -> relational (Type.Float o) o
    Builtin.LtInt -> relational (Type.Int o) o

    Builtin.ModFloat -> binary (Type.Float o) o
    Builtin.ModInt -> binary (Type.Int o) o

    Builtin.MulFloat -> binary (Type.Float o) o
    Builtin.MulInt -> binary (Type.Int o) o

    Builtin.NeFloat -> relational (Type.Float o) o
    Builtin.NeInt -> relational (Type.Int o) o

    Builtin.NegFloat -> unary (Type.Float o) o
    Builtin.NegInt -> unary (Type.Int o) o

    Builtin.None -> forAll $ \r a
      -> (r --> r :. (a :?)) o

    Builtin.NotBool -> unary (Type.Bool o) o
    Builtin.NotInt -> unary (Type.Int o) o

    Builtin.OpenIn -> forAll $ \r
      -> (r :. string o --> r :. Type.Handle o) o

    Builtin.OpenOut -> forAll $ \r
      -> (r :. string o --> r :. Type.Handle o) o

    Builtin.Option -> forAll $ \r a -> Type.Function
      (r :. (a :?) :. Type.Function (r :. a) r o) r o

    Builtin.OptionElse -> forAll $ \r a s -> Type.Function
      (r :. (a :?)
        :. Type.Function (r :. a) s o
        :. Type.Function r s o)
      s o

    Builtin.OrBool -> binary (Type.Bool o) o
    Builtin.OrInt -> binary (Type.Int o) o

    Builtin.Rest -> forAll $ \r a b
      -> (r :. a :& b --> r :. b) o

    Builtin.Right -> forAll $ \r a b
      -> (r :. b --> r :. a :| b) o

    Builtin.Set -> forAll $ \r a
      -> (r :. Type.Vector a o :. a :. Type.Int o
      --> r :. Type.Vector a o) o

    Builtin.ShowFloat -> forAll $ \r
      -> (r :. Type.Float o --> r :. string o) o

    Builtin.ShowInt -> forAll $ \r
      -> (r :. Type.Int o --> r :. string o) o

    Builtin.Some -> forAll $ \r a
      -> (r :. a --> r :. (a :?)) o

    Builtin.Stderr -> forAll $ \r
      -> (r --> r :. Type.Handle o) o
    Builtin.Stdin -> forAll $ \r
      -> (r --> r :. Type.Handle o) o
    Builtin.Stdout -> forAll $ \r
      -> (r --> r :. Type.Handle o) o

    Builtin.SubFloat -> binary (Type.Float o) o
    Builtin.SubInt -> binary (Type.Int o) o

    Builtin.Pair -> forAll $ \r a b
      -> (r :. a :. b --> r :. a :& b) o

    Builtin.Print -> forAll $ \r
      -> (r :. string o :. Type.Handle o --> r) o

    Builtin.Tail -> forAll $ \r a
      -> (r :. Type.Vector a o --> r :. Type.Vector a o) o

    Builtin.UnsafePurify11 -> forAll $ \r s a b
      -> (r :. (s :. a --> s :. b) o --> r :. (s :. a --> s :. b) o) o

    Builtin.XorBool -> binary (Type.Bool o) o
    Builtin.XorInt -> binary (Type.Int o) o

    where
    o :: Origin
    o = Origin (AnnoType (Kitten.Type.Builtin name)) loc

  Call name loc -> asTyped (Typed.Call name) loc
    $ instantiateM =<< declOrDef
    where
    declOrDef = do
      decls <- getsEnv envDecls
      case N.lookup name decls of
        Just decl -> return decl
        Nothing -> getsEnv ((N.! name) . envDefs)

  Compose terms loc -> withLocation loc $ do
    (typedTerms, types) <- V.mapAndUnzipM recur terms
    r <- freshVarM
    origin <- getsEnv envOrigin
    type_ <- foldM
      (\(Type.Function a b _) (Type.Function c d _)
        -> inferCompose a b c d)
      ((r --> r) origin)
      (V.toList types)
    return (Typed.Compose typedTerms loc (sub finalEnv type_), type_)

  From name loc -> asTyped (Typed.From name) loc $ do
    underlying <- instantiateM =<< getsEnv ((M.! name) . envTypeDefs)
    origin <- getsEnv envOrigin
    forAll $ \r -> (r :. Type.Named name origin --> r :. underlying) origin

  PairTerm x y loc -> withLocation loc $ do
    (x', a) <- secondM fromConstant =<< recur x
    (y', b) <- secondM fromConstant =<< recur y
    origin <- getsEnv envOrigin
    type_ <- forAll $ \r -> (r --> r :. a :& b) origin
    return (Typed.PairTerm x' y' loc (sub finalEnv type_), type_)

  Push value loc -> withLocation loc $ do
    (value', a) <- inferValue finalEnv value
    origin <- getsEnv envOrigin
    type_ <- forAll $ \r -> (r --> r :. a) origin
    return (Typed.Push value' loc (sub finalEnv type_), type_)

  Scoped name term loc -> withOrigin (Origin (Type.Local name) loc) $ do
    a <- freshVarM
    (term', Type.Function b c _) <- local a $ recur term
    origin <- getsEnv envOrigin
    let type_ = Type.Function (b :. a) c origin
    return (Typed.Scoped term' loc (sub finalEnv type_), type_)

  To name loc -> asTyped (Typed.To name) loc $ do
    underlying <- instantiateM =<< getsEnv ((M.! name) . envTypeDefs)
    origin <- getsEnv envOrigin
    forAll $ \r -> (r :. underlying --> r :. Type.Named name origin) origin

  VectorTerm values loc -> withLocation loc $ do
    (typedValues, types) <- mapAndUnzipM recur (V.toList values)
    elementType <- fromConstant =<< unifyEach types
    origin <- getsEnv envOrigin
    type_ <- forAll $ \r -> (r --> r :. Type.Vector elementType origin) origin
    return (Typed.VectorTerm (V.fromList typedValues) loc (sub finalEnv type_), type_)

  where
  recur = infer finalEnv

  asTyped
    :: (Location -> Type Scalar -> a)
    -> Location
    -> Inferred (Type Scalar)
    -> Inferred (a, Type Scalar)
  asTyped constructor loc action = do
    type_ <- withLocation loc action
    return (constructor loc (sub finalEnv type_), type_)

fromConstant :: Type Scalar -> Inferred (Type Scalar)
fromConstant type_ = do
  a <- freshVarM
  r <- freshVarM
  origin <- getsEnv envOrigin
  type_ === (r --> r :. a) origin
  return a

binary :: Type Scalar -> Origin -> Inferred (Type Scalar)
binary a origin = forAll
  $ \r -> (r :. a :. a --> r :. a) origin

relational :: Type Scalar -> Origin -> Inferred (Type Scalar)
relational a origin = forAll
  $ \r -> (r :. a :. a --> r :. Type.Bool origin) origin

unary :: Type Scalar -> Origin -> Inferred (Type Scalar)
unary a origin = forAll
  $ \r -> (r :. a --> r :. a) origin

string :: Origin -> Type Scalar
string origin = Type.Vector (Type.Char origin) origin

local :: Type Scalar -> Inferred a -> Inferred a
local type_ action = do
  modifyEnv $ \env -> env { envLocals = type_ : envLocals env }
  result <- action
  modifyEnv $ \env -> env { envLocals = tail $ envLocals env }
  return result

withClosure :: Vector (Type Scalar) -> Inferred a -> Inferred a
withClosure types action = do
  original <- getsEnv envClosure
  modifyEnv $ \env -> env { envClosure = types }
  result <- action
  modifyEnv $ \env -> env { envClosure = original }
  return result

getClosedName :: ClosedName -> Inferred (Type Scalar)
getClosedName name = case name of
  ClosedName (Name index) -> getsEnv $ (!! index) . envLocals
  ReclosedName (Name index) -> getsEnv $ (V.! index) . envClosure

inferValue :: Env -> Value -> Inferred (Typed.Value, Type Scalar)
inferValue finalEnv value = getsEnv envOrigin >>= \origin -> case value of
  Bool val -> return (Typed.Bool val, Type.Bool origin)
  Char val -> return (Typed.Char val, Type.Char origin)
  Closed name@(Name index) -> do
    type_ <- getsEnv ((V.! index) . envClosure)
    return (Typed.Closed name, type_)
  Closure names term -> do
    closedTypes <- V.mapM getClosedName names
    (term', type_) <- withClosure closedTypes (infer finalEnv term)
    return (Typed.Closure names term', type_)
  Float val -> return (Typed.Float val, Type.Float origin)
  Function _ -> error "function appeared during type inference"
  Int val -> return (Typed.Int val, Type.Int origin)
  Local name@(Name index) -> do
    type_ <- getsEnv ((!! index) . envLocals)
    return (Typed.Local name, type_)
  Unit -> return (Typed.Unit, Type.Unit origin)
  String val -> return
    (Typed.String val, Type.Vector (Type.Char origin) origin)

unifyEach :: [Type Scalar] -> Inferred (Type Scalar)
unifyEach (x : y : zs) = x === y >> unifyEach (y : zs)
unifyEach [x] = return x
unifyEach [] = freshVarM

inferCompose
  :: Type Row -> Type Row
  -> Type Row -> Type Row
  -> Inferred (Type Scalar)
inferCompose in1 out1 in2 out2 = do
  out1 === in2
  Type.Function in1 out2 <$> getsEnv envOrigin
