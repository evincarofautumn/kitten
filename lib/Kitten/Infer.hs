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
import Kitten.Type hiding (Type(..))
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
  -> [Type Scalar]
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
  -> [Type Scalar]
  -> Inferred (Fragment Typed, Type Scalar)
inferFragment prelude fragment stackTypes = mdo

  -- Aggregate type definitions ('type Foo Bar').
  F.forM_ allTypeDefs $ \typeDef -> do
    let name = typeDefName typeDef
    scheme <- fromAnno (typeDefAnno typeDef)
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
    -> withLocation (defLocation def) $ do
      (Typed.Closure _ typedTerm, type_) <- inferValue finalEnv (defTerm def)
      typeScheme <- generalize type_
      declaredScheme <- do
        decls <- getsEnv envDecls
        case N.lookup (Name index) decls of
          Just decl -> return decl
          Nothing -> do
            loc <- getsEnv envLocation
            mono <$> forAll (\r s e -> Type.Function r s e loc)
      saveDefWith (flip const) index typeScheme
      declared <- instantiateM declaredScheme
      inferred <- instantiateM typeScheme
      declared === inferred
      return def { defTerm = typedTerm <$ typeScheme }

  (typedTerms, fragmentType) <- infer finalEnv
    $ Compose (fragmentTerms fragment) UnknownLocation

  -- Equate the bottom of the stack with stackTypes.
  do
    let Type.Function consumption _ _ _ = fragmentType
    bottom <- freshVarM
    enforce <- asksConfig enforceBottom
    when enforce $ bottom === Type.Empty UnknownLocation
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

  preludeIndices, fragmentIndices :: [Int]
  (preludeIndices, fragmentIndices) = splitAt
    (V.length (fragmentDefs prelude)) [0..]

  iforFragmentDefs
    :: (Int -> Def Value -> Inferred a)
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
      scheme <- fromAnno anno
      saveDecl index scheme
      saveDefWith const index scheme
    Nothing -> saveDecl index =<< mono
      <$> forAll (\r s e -> Type.Function r s e (defLocation def))

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

  Builtin name loc -> asTyped (Typed.Builtin name) loc $ case name of

    Builtin.AddVector -> forAll $ \r a
      -> (r :. Type.Vector a loc :. Type.Vector a loc
      --> r :. Type.Vector a loc) loc

    Builtin.AddFloat -> binary (Type.Float loc) loc

    Builtin.AddInt -> binary (Type.Int loc) loc

    Builtin.AndBool -> binary (Type.Bool loc) loc

    Builtin.AndInt -> binary (Type.Int loc) loc

    Builtin.Apply -> forAll $ \r s e
      -> Type.Function (r :. Type.Function r s e loc) s e loc

    Builtin.CharToInt -> forAll $ \r
      -> (r :. Type.Char loc --> r :. Type.Int loc) loc

    Builtin.Choice -> forAll $ \r a b e -> Type.Function
      (r :. (a :| b) :. Type.Function (r :. a) r e loc) r e loc

    Builtin.ChoiceElse -> forAll $ \r a b s e1 e2 -> Type.Function
      (r :. (a :| b)
        :. Type.Function (r :. a) s e1 loc
        :. Type.Function (r :. b) s e2 loc)
      s (e1 +: e2) loc

    Builtin.Close -> forAll $ \r
      -> (r :. Type.Handle loc ==> r) loc

    Builtin.DivFloat -> binary (Type.Float loc) loc
    Builtin.DivInt -> binary (Type.Int loc) loc

    Builtin.EqFloat -> relational (Type.Float loc) loc
    Builtin.EqInt -> relational (Type.Int loc) loc

    Builtin.Exit -> forAll $ \r
      -> (r :. Type.Int loc ==> r) loc

    Builtin.First -> forAll $ \r a b
      -> (r :. a :& b --> r :. a) loc

    Builtin.FromLeft -> forAll $ \r a b
      -> (r :. a :| b --> r :. a) loc

    Builtin.FromRight -> forAll $ \r a b
      -> (r :. a :| b --> r :. b) loc

    Builtin.FromSome -> forAll $ \r a
      -> (r :. (a :?) --> r :. a) loc

    Builtin.GeFloat -> relational (Type.Float loc) loc
    Builtin.GeInt -> relational (Type.Int loc) loc

    Builtin.Get -> forAll $ \r a
      -> (r :. Type.Vector a loc :. Type.Int loc --> r :. (a :?)) loc

    Builtin.GetLine -> forAll $ \r
      -> (r :. Type.Handle loc ==> r :. string loc) loc

    Builtin.GtFloat -> relational (Type.Float loc) loc
    Builtin.GtInt -> relational (Type.Int loc) loc

    Builtin.If -> forAll $ \r e -> Type.Function
      (r :. Type.Bool loc :. Type.Function r r e loc) r e loc

    Builtin.IfElse -> forAll $ \r s e1 e2 -> Type.Function
      (r :. Type.Bool loc
        :. Type.Function r s e1 loc
        :. Type.Function r s e2 loc)
      s (e1 +: e2) loc

    Builtin.Impure -> forAll $ \r
      -> (r ==> r) loc

    Builtin.Init -> forAll $ \r a
      -> (r :. Type.Vector a loc --> r :. Type.Vector a loc) loc

    Builtin.IntToChar -> forAll $ \r
      -> (r :. Type.Int loc --> r :. (Type.Char loc :?)) loc

    Builtin.LeFloat -> relational (Type.Float loc) loc
    Builtin.LeInt -> relational (Type.Int loc) loc

    Builtin.Left -> forAll $ \r a b
      -> (r :. a --> r :. a :| b) loc

    Builtin.Length -> forAll $ \r a
      -> (r :. Type.Vector a loc --> r :. Type.Int loc) loc

    Builtin.LtFloat -> relational (Type.Float loc) loc
    Builtin.LtInt -> relational (Type.Int loc) loc

    Builtin.ModFloat -> binary (Type.Float loc) loc
    Builtin.ModInt -> binary (Type.Int loc) loc

    Builtin.MulFloat -> binary (Type.Float loc) loc
    Builtin.MulInt -> binary (Type.Int loc) loc

    Builtin.NeFloat -> relational (Type.Float loc) loc
    Builtin.NeInt -> relational (Type.Int loc) loc

    Builtin.NegFloat -> unary (Type.Float loc) loc
    Builtin.NegInt -> unary (Type.Int loc) loc

    Builtin.None -> forAll $ \r a
      -> (r --> r :. (a :?)) loc

    Builtin.NotBool -> unary (Type.Bool loc) loc
    Builtin.NotInt -> unary (Type.Int loc) loc

    Builtin.OpenIn -> forAll $ \r
      -> (r :. string loc ==> r :. Type.Handle loc) loc

    Builtin.OpenOut -> forAll $ \r
      -> (r :. string loc ==> r :. Type.Handle loc) loc

    Builtin.Option -> forAll $ \r a e -> Type.Function
      (r :. (a :?) :. Type.Function (r :. a) r e loc) r e loc

    Builtin.OptionElse -> forAll $ \r a s e1 e2 -> Type.Function
      (r :. (a :?)
        :. Type.Function (r :. a) s e1 loc
        :. Type.Function r s e2 loc)
      s (e1 +: e2) loc

    Builtin.OrBool -> binary (Type.Bool loc) loc
    Builtin.OrInt -> binary (Type.Int loc) loc

    Builtin.Rest -> forAll $ \r a b
      -> (r :. a :& b --> r :. b) loc

    Builtin.Right -> forAll $ \r a b
      -> (r :. b --> r :. a :| b) loc

    Builtin.Set -> forAll $ \r a
      -> (r :. Type.Vector a loc :. a :. Type.Int loc
      --> r :. Type.Vector a loc) loc

    Builtin.ShowFloat -> forAll $ \r
      -> (r :. Type.Float loc --> r :. string loc) loc

    Builtin.ShowInt -> forAll $ \r
      -> (r :. Type.Int loc --> r :. string loc) loc

    Builtin.Some -> forAll $ \r a
      -> (r :. a --> r :. (a :?)) loc

    Builtin.Stderr -> forAll $ \r
      -> (r --> r :. Type.Handle loc) loc
    Builtin.Stdin -> forAll $ \r
      -> (r --> r :. Type.Handle loc) loc
    Builtin.Stdout -> forAll $ \r
      -> (r --> r :. Type.Handle loc) loc

    Builtin.SubFloat -> binary (Type.Float loc) loc
    Builtin.SubInt -> binary (Type.Int loc) loc

    Builtin.Pair -> forAll $ \r a b
      -> (r :. a :. b --> r :. a :& b) loc

    Builtin.Print -> forAll $ \r
      -> (r :. string loc :. Type.Handle loc ==> r) loc

    Builtin.Tail -> forAll $ \r a
      -> (r :. Type.Vector a loc --> r :. Type.Vector a loc) loc

    Builtin.UnsafePurify11 -> forAll $ \r s a b
      -> (r :. (s :. a ==> s :. b) loc --> r :. (s :. a --> s :. b) loc) loc

    Builtin.XorBool -> binary (Type.Bool loc) loc
    Builtin.XorInt -> binary (Type.Int loc) loc

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
    type_ <- foldM
      (\(Type.Function a b e1 _) (Type.Function c d e2 _)
        -> inferCompose a b c d e1 e2)
      ((r --> r) loc)
      (V.toList types)
    return (Typed.Compose typedTerms loc (sub finalEnv type_), type_)

  From name loc -> asTyped (Typed.From name) loc $ do
    underlying <- instantiateM =<< getsEnv ((M.! name) . envTypeDefs)
    forAll $ \r -> (r :. Type.Named name loc --> r :. underlying) loc

  PairTerm x y loc -> withLocation loc $ do
    (x', a) <- secondM fromConstant =<< recur x
    (y', b) <- secondM fromConstant =<< recur y
    type_ <- forAll $ \r -> (r --> r :. a :& b) loc
    return (Typed.PairTerm x' y' loc (sub finalEnv type_), type_)

  Push value loc -> withLocation loc $ do
    (value', a) <- inferValue finalEnv value
    type_ <- forAll $ \r -> (r --> r :. a) loc
    return (Typed.Push value' loc (sub finalEnv type_), type_)

  Scoped term loc -> withLocation loc $ do
    a <- freshVarM
    (term', Type.Function b c e _) <- local a $ recur term
    let type_ = Type.Function (b :. a) c e loc
    return (Typed.Scoped term' loc (sub finalEnv type_), type_)

  To name loc -> asTyped (Typed.To name) loc $ do
    underlying <- instantiateM =<< getsEnv ((M.! name) . envTypeDefs)
    forAll $ \r -> (r :. underlying --> r :. Type.Named name loc) loc

  VectorTerm values loc -> withLocation loc $ do
    (typedValues, types) <- mapAndUnzipM recur (V.toList values)
    elementType <- fromConstant =<< unifyEach types
    type_ <- forAll $ \r -> (r --> r :. Type.Vector elementType loc) loc
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
  loc <- getsEnv envLocation
  type_ === (r --> r :. a) loc
  return a

binary :: Type Scalar -> Location -> Inferred (Type Scalar)
binary a loc = forAll $ \r -> (r :. a :. a --> r :. a) loc

relational :: Type Scalar -> Location -> Inferred (Type Scalar)
relational a loc = forAll $ \r -> (r :. a :. a --> r :. Type.Bool loc) loc

unary :: Type Scalar -> Location -> Inferred (Type Scalar)
unary a loc = forAll $ \r -> (r :. a --> r :. a) loc

string :: Location -> Type Scalar
string loc = Type.Vector (Type.Char loc) loc

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
inferValue finalEnv value = getsEnv envLocation >>= \loc -> case value of
  Bool val -> return (Typed.Bool val, Type.Bool loc)
  Char val -> return (Typed.Char val, Type.Char loc)
  Closed name@(Name index) -> do
    type_ <- getsEnv ((V.! index) . envClosure)
    return (Typed.Closed name, type_)
  Closure names term -> do
    closedTypes <- V.mapM getClosedName names
    (term', type_) <- withClosure closedTypes (infer finalEnv term)
    return (Typed.Closure names term', type_)
  Float val -> return (Typed.Float val, Type.Float loc)
  Function _ -> error "function appeared during type inference"
  Int val -> return (Typed.Int val, Type.Int loc)
  Local name@(Name index) -> do
    type_ <- getsEnv ((!! index) . envLocals)
    return (Typed.Local name, type_)
  Unit -> return (Typed.Unit, Type.Unit loc)
  String val -> return
    (Typed.String val, Type.Vector (Type.Char loc) loc)

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
