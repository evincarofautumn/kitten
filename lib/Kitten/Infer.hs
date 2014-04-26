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
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Traversable as T
import qualified Data.Vector as V

import Kitten.Error
import Kitten.Infer.Monad
import Kitten.Infer.Scheme
import Kitten.Infer.Type
import Kitten.Infer.Unify
import Kitten.Location
import Kitten.Types
import Kitten.Util.FailWriter
import Kitten.Util.Monad
import Kitten.Util.Text (toText)

import qualified Kitten.Util.Vector as V

typeFragment
  :: Vector (Type Scalar)
  -> Fragment ResolvedTerm
  -> K (Fragment TypedTerm, Type Scalar)
typeFragment stackTypes fragment
  = inferFragment stackTypes fragment

inferFragment
  :: Vector (Type Scalar)
  -> Fragment ResolvedTerm
  -> K (Fragment TypedTerm, Type Scalar)
inferFragment stackTypes fragment = do
  rec
    -- Populate environment with definition types.
    --
    -- FIXME(strager):
    -- Use previously-inferred types (i.e. defTypeScheme def). We cannot
    -- right now because effects are not inferred properly (e.g. for the
    -- effects of the 'map' prelude function).
    _ <- T.mapM save (fragmentDefs fragment)

    typedDefs <- flip T.mapM (fragmentDefs fragment) $ \def
      -> withOrigin (defOrigin def) $ do
        (typedTerm, inferredScheme) <- generalize
          (infer finalProgram (unscheme (defTerm def)))  -- See note [scheming defs].
        declaredScheme <- do
          decls <- getsProgram inferenceDecls
          case H.lookup (defName def) decls of
            Just decl -> return decl
            Nothing -> do
              origin <- getsProgram inferenceOrigin
              fmap mono . forAll $ \r s -> TyFunction r s origin
        saveDefWith (flip const) (defName def) inferredScheme
        instanceCheck inferredScheme declaredScheme $ let
          item = CompileError (defLocation def)
          in ErrorGroup
          [ item Error $ T.unwords
            [ "inferred type of"
            , defName def
            , "is not an instance of its declared type"
            ]
          , item Note $ T.unwords ["inferred", toText inferredScheme]
          , item Note $ T.unwords ["declared", toText declaredScheme]
          ]
        return def { defTerm = typedTerm <$ inferredScheme }

    topLevel <- getsProgram (originLocation . inferenceOrigin)
    (typedTerms, fragmentType) <- infer finalProgram
      $ TrCompose StackAny (fragmentTerms fragment) topLevel

    -- Equate the bottom of the stack with stackTypes.
    do
      let TyFunction consumption _ _ = fragmentType
      bottom <- freshVarM
      -- TODO Reinstate this check.
      -- enforce <- asksConfig enforceBottom
      -- when enforce $ bottom === TyEmpty (Origin NoHint topLevel)
      let stackType = F.foldl (:.) bottom stackTypes
      stackType === consumption

    let
      typedFragment = fragment
        { fragmentDefs = typedDefs
        , fragmentTerms = V.singleton typedTerms
        }

    finalProgram <- getProgram

  return (typedFragment, sub finalProgram fragmentType)

  where
  defOrigin :: Def a -> Origin
  defOrigin def = Origin
    (HiType (AnDef (defName def)))
    (defLocation def)

  saveDecl :: Text -> TypeScheme -> K ()
  saveDecl name scheme = modifyProgram $ \program -> program
    { inferenceDecls = H.insert name scheme (inferenceDecls program) }

  saveDefWith
    :: (TypeScheme -> TypeScheme -> TypeScheme)
    -> Text
    -> TypeScheme
    -> K ()
  saveDefWith f name scheme = modifyProgram $ \program -> program
    { inferenceDefs = H.insertWith f name scheme (inferenceDefs program) }

  save :: Def a -> K ()
  save def = do
    scheme <- fromAnno (AnDef (defName def)) (defAnno def)
    saveDecl (defName def) scheme
    saveDefWith const (defName def) scheme

instanceCheck :: TypeScheme -> TypeScheme -> ErrorGroup -> K ()
instanceCheck inferredScheme declaredScheme errorGroup = do
  inferredType <- instantiateM inferredScheme
  (stackConsts, scalarConsts, declaredType) <- skolemize declaredScheme
  inferredType === declaredType
  let
    (escapedStacks, escapedScalars) = free inferredScheme <> free declaredScheme
    badStacks = filter (`elem` escapedStacks) stackConsts
    badScalars = filter (`elem` escapedScalars) scalarConsts
  unless (null badStacks && null badScalars)
    $ liftFailWriter $ throwMany [errorGroup]

-- Note [scheming defs]:
--
-- Defs are always wrapped in 'Scheme's for convenience after type
-- inference, but they are always monomorphic beforehand.

class ForAll a b where
  forAll :: a -> K (Type b)

instance ForAll a b => ForAll (Type c -> a) b where
  forAll f = do
    var <- freshVarM
    forAll $ f var

instance ForAll (Type a) a where
  forAll = pure

-- | Infers the type of a term.
infer :: Program -> ResolvedTerm -> K (TypedTerm, Type Scalar)
infer finalProgram resolved = case resolved of

  TrCall hint name loc -> asTyped (TrCall hint name) loc
    $ instantiateM =<< declOrDef
    where
    declOrDef = do
      decls <- getsProgram inferenceDecls
      case H.lookup name decls of
        Just decl -> return decl
        Nothing -> getsProgram ((H.! name) . inferenceDefs)

  TrCompose hint terms loc -> withLocation loc $ do
    (typedTerms, types) <- V.mapAndUnzipM recur terms
    r <- freshVarM
    origin <- getsProgram inferenceOrigin
    let
      composed = foldM
        (\x y -> do
          TyFunction a b _ <- unquantify x
          TyFunction c d _ <- unquantify y
          inferCompose a b c d)
        ((r --> r) origin)
        (V.toList types)

    -- We need the generalized type to check stack effects.
    (_, typeScheme) <- generalize $ (,) () <$> composed
    type_ <- composed

    -- Check stack effect hint.
    case hint of
      Stack0 -> do
        s <- freshKindedIdM
        instanceCheck typeScheme
          (Forall (S.singleton s) S.empty
            $ (TyVar s origin --> TyVar s origin) origin)
          $ ErrorGroup
          [ CompileError loc Error $ T.unwords
            [ toText typeScheme
            , "has a non-null stack effect"
            ]
          ]
      Stack1 -> do
        s <- freshKindedIdM
        a <- freshKindedIdM
        -- Note that 'a' is not forall-bound. We want this effect hint
        -- to match function types that produce a single result of any
        -- type, and that operate on any input stack; but these two
        -- notions of "any" are quite different. In the former case, we
        -- care very much that the stack type is immaterial. In the
        -- latter, we don't care what the type is at all.
        instanceCheck typeScheme
          (Forall (S.singleton s) S.empty
            $ (TyVar s origin
              --> TyVar s origin :. TyVar a origin) origin)
          $ ErrorGroup
          [ CompileError loc Error $ T.unwords
            [ toText typeScheme
            , "has a non-unary stack effect"
            ]
          ]
      StackAny -> noop

    return (TrCompose hint typedTerms (loc, sub finalProgram type_), type_)

  TrIntrinsic name loc -> asTyped (TrIntrinsic name) loc $ case name of

    InAddVector -> forAll $ \r a
      -> (r :. TyVector a o :. TyVector a o
      --> r :. TyVector a o) o

    InAddFloat -> binary (tyFloat o) o

    InAddInt -> binary (tyInt o) o

    InAndBool -> binary (tyBool o) o

    InAndInt -> binary (tyInt o) o

    InApply -> forAll $ \r s
      -> TyFunction (r :. TyFunction r s o) s o

    InCharToInt -> forAll $ \r
      -> (r :. tyChar o --> r :. tyInt o) o

    InChoice -> forAll $ \r a b -> TyFunction
      (r :. (a :| b) :. TyFunction (r :. a) r o) r o

    InChoiceElse -> forAll $ \r a b s -> TyFunction
      (r :. (a :| b)
        :. TyFunction (r :. a) s o
        :. TyFunction (r :. b) s o)
      s o

    InClose -> forAll $ \r
      -> (r :. tyHandle o --> r) o

    InDivFloat -> binary (tyFloat o) o
    InDivInt -> binary (tyInt o) o

    InEqFloat -> relational (tyFloat o) o
    InEqInt -> relational (tyInt o) o

    InExit -> forAll $ \r s
      -> (r :. tyInt o --> s) o

    InFirst -> forAll $ \r a b
      -> (r :. a :& b --> r :. a) o

    InFromLeft -> forAll $ \r a b
      -> (r :. a :| b --> r :. a) o

    InFromRight -> forAll $ \r a b
      -> (r :. a :| b --> r :. b) o

    InFromSome -> forAll $ \r a
      -> (r :. (a :?) --> r :. a) o

    InGeFloat -> relational (tyFloat o) o
    InGeInt -> relational (tyInt o) o

    InGet -> forAll $ \r a
      -> (r :. TyVector a o :. tyInt o --> r :. (a :?)) o

    InGetLine -> forAll $ \r
      -> (r :. tyHandle o --> r :. string o) o

    InGtFloat -> relational (tyFloat o) o
    InGtInt -> relational (tyInt o) o

    InIf -> forAll $ \r -> TyFunction
      (r :. tyBool o :. TyFunction r r o) r o

    InIfElse -> forAll $ \r s -> TyFunction
      (r :. tyBool o
        :. TyFunction r s o
        :. TyFunction r s o)
      s o

    InInit -> forAll $ \r a
      -> (r :. TyVector a o --> r :. TyVector a o) o

    InIntToChar -> forAll $ \r
      -> (r :. tyInt o --> r :. (tyChar o :?)) o

    InLeFloat -> relational (tyFloat o) o
    InLeInt -> relational (tyInt o) o

    InLeft -> forAll $ \r a b
      -> (r :. a --> r :. a :| b) o

    InLength -> forAll $ \r a
      -> (r :. TyVector a o --> r :. tyInt o) o

    InLtFloat -> relational (tyFloat o) o
    InLtInt -> relational (tyInt o) o

    InModFloat -> binary (tyFloat o) o
    InModInt -> binary (tyInt o) o

    InMulFloat -> binary (tyFloat o) o
    InMulInt -> binary (tyInt o) o

    InNeFloat -> relational (tyFloat o) o
    InNeInt -> relational (tyInt o) o

    InNegFloat -> unary (tyFloat o) o
    InNegInt -> unary (tyInt o) o

    InNone -> forAll $ \r a
      -> (r --> r :. (a :?)) o

    InNotBool -> unary (tyBool o) o
    InNotInt -> unary (tyInt o) o

    InOpenIn -> forAll $ \r
      -> (r :. string o --> r :. tyHandle o) o

    InOpenOut -> forAll $ \r
      -> (r :. string o --> r :. tyHandle o) o

    InOption -> forAll $ \r a -> TyFunction
      (r :. (a :?) :. TyFunction (r :. a) r o) r o

    InOptionElse -> forAll $ \r a s -> TyFunction
      (r :. (a :?)
        :. TyFunction (r :. a) s o
        :. TyFunction r s o)
      s o

    InOrBool -> binary (tyBool o) o
    InOrInt -> binary (tyInt o) o

    InRest -> forAll $ \r a b
      -> (r :. a :& b --> r :. b) o

    InRight -> forAll $ \r a b
      -> (r :. b --> r :. a :| b) o

    InSet -> forAll $ \r a
      -> (r :. TyVector a o :. tyInt o :. a
      --> r :. TyVector a o) o

    InShowFloat -> forAll $ \r
      -> (r :. tyFloat o --> r :. string o) o

    InShowInt -> forAll $ \r
      -> (r :. tyInt o --> r :. string o) o

    InSome -> forAll $ \r a
      -> (r :. a --> r :. (a :?)) o

    InStderr -> forAll $ \r
      -> (r --> r :. tyHandle o) o
    InStdin -> forAll $ \r
      -> (r --> r :. tyHandle o) o
    InStdout -> forAll $ \r
      -> (r --> r :. tyHandle o) o

    InSubFloat -> binary (tyFloat o) o
    InSubInt -> binary (tyInt o) o

    InPair -> forAll $ \r a b
      -> (r :. a :. b --> r :. a :& b) o

    InPrint -> forAll $ \r
      -> (r :. string o :. tyHandle o --> r) o

    InTail -> forAll $ \r a
      -> (r :. TyVector a o --> r :. TyVector a o) o

    InXorBool -> binary (tyBool o) o
    InXorInt -> binary (tyInt o) o

    where
    o :: Origin
    o = Origin (HiType (AnIntrinsic name)) loc

  TrLambda name term loc -> withOrigin (Origin (HiLocal name) loc) $ do
    a <- freshVarM
    (term', TyFunction b c _) <- local a $ recur term
    origin <- getsProgram inferenceOrigin
    let type_ = TyFunction (b :. a) c origin
    return (TrLambda name term' (loc, sub finalProgram type_), type_)

  TrMakePair x y loc -> withLocation loc $ do
    (x', a) <- secondM fromConstant =<< recur x
    (y', b) <- secondM fromConstant =<< recur y
    origin <- getsProgram inferenceOrigin
    type_ <- forAll $ \r -> (r --> r :. a :& b) origin
    return (TrMakePair x' y' (loc, sub finalProgram type_), type_)

  TrPush value loc -> withLocation loc $ do
    (value', a) <- inferValue finalProgram value
    origin <- getsProgram inferenceOrigin
    type_ <- forAll $ \r -> (r --> r :. a) origin
    return (TrPush value' (loc, sub finalProgram type_), type_)

  TrMakeVector values loc -> withLocation loc $ do
    (typedValues, types) <- mapAndUnzipM recur (V.toList values)
    elementType <- fromConstant =<< unifyEach types
    origin <- getsProgram inferenceOrigin
    type_ <- forAll $ \r -> (r --> r :. TyVector elementType origin) origin
    return
      ( TrMakeVector (V.fromList typedValues) (loc, sub finalProgram type_)
      , type_
      )

  where
  recur = infer finalProgram

  asTyped
    :: ((Location, Type Scalar) -> a)
    -> Location
    -> K (Type Scalar)
    -> K (a, Type Scalar)
  asTyped constructor loc action = do
    type_ <- withLocation loc action
    return (constructor (loc, sub finalProgram type_), type_)

-- | Removes top-level quantifiers from a type.
unquantify :: Type a -> K (Type a)
unquantify (TyQuantified scheme _) = unquantify =<< instantiateM scheme
unquantify type_ = return type_

fromConstant :: Type Scalar -> K (Type Scalar)
fromConstant type_ = do
  a <- freshVarM
  r <- freshVarM
  origin <- getsProgram inferenceOrigin
  type_ === (r --> r :. a) origin
  return a

binary :: Type Scalar -> Origin -> K (Type Scalar)
binary a origin = forAll
  $ \r -> (r :. a :. a --> r :. a) origin

relational :: Type Scalar -> Origin -> K (Type Scalar)
relational a origin = forAll
  $ \r -> (r :. a :. a --> r :. tyBool origin) origin

unary :: Type Scalar -> Origin -> K (Type Scalar)
unary a origin = forAll
  $ \r -> (r :. a --> r :. a) origin

string :: Origin -> Type Scalar
string origin = TyVector (tyChar origin) origin

local :: Type Scalar -> K a -> K a
local type_ action = do
  modifyProgram $ \program -> program { inferenceLocals = type_ : inferenceLocals program }
  result <- action
  modifyProgram $ \program -> program { inferenceLocals = tail $ inferenceLocals program }
  return result

withClosure :: Vector (Type Scalar) -> K a -> K a
withClosure types action = do
  original <- getsProgram inferenceClosure
  modifyProgram $ \program -> program { inferenceClosure = types }
  result <- action
  modifyProgram $ \program -> program { inferenceClosure = original }
  return result

getClosedName :: ClosedName -> K (Type Scalar)
getClosedName name = case name of
  ClosedName index -> getsProgram $ (!! index) . inferenceLocals
  ReclosedName index -> getsProgram $ (V.! index) . inferenceClosure

inferValue :: Program -> ResolvedValue -> K (TypedValue, Type Scalar)
inferValue finalProgram value = getsProgram inferenceOrigin >>= \origin -> case value of
  TrBool val loc -> ret loc (tyBool origin) (TrBool val)
  TrChar val loc -> ret loc (tyChar origin) (TrChar val)
  TrClosed index loc -> do
    type_ <- getsProgram ((V.! index) . inferenceClosure)
    ret loc type_ (TrClosed index)
  TrClosure names term loc -> do
    closedTypes <- V.mapM getClosedName names
    (term', type_) <- withClosure closedTypes (infer finalProgram term)
    ret loc type_ (TrClosure names term')
  TrFloat val loc -> ret loc (tyFloat origin) (TrFloat val)
  TrInt val loc -> ret loc (tyInt origin) (TrInt val)
  TrLocal index loc -> do
    type_ <- getsProgram ((!! index) . inferenceLocals)
    ret loc type_ (TrLocal index)
  TrQuotation{} -> error "quotation appeared during type inference"
  TrText val loc -> ret loc (TyVector (tyChar origin) origin) (TrText val)
  where
  ret loc type_ constructor = return (constructor (loc, type_), type_)

unifyEach :: [Type Scalar] -> K (Type Scalar)
unifyEach (x : y : zs) = x === y >> unifyEach (y : zs)
unifyEach [x] = return x
unifyEach [] = freshVarM

inferCompose
  :: Type Stack -> Type Stack
  -> Type Stack -> Type Stack
  -> K (Type Scalar)
inferCompose in1 out1 in2 out2 = do
  out1 === in2
  TyFunction in1 out2 <$> getsProgram inferenceOrigin
