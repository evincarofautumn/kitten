module Kitten.Infer
  ( infer
  , typeFragment
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid
import Data.Map ((!))

import qualified Data.Map as Map

import Kitten.ClosedName
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Infer.Monad
import Kitten.Infer.Scheme
import Kitten.Infer.Type
import Kitten.Infer.Unify
import Kitten.Name
import Kitten.Resolved (Resolved)
import Kitten.Type
import Kitten.Typed
import Kitten.Util.FailWriter
import Kitten.Util.Void

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Resolved as Resolved

typeFragment
  :: Fragment Value Void
  -> Fragment Resolved.Value Resolved
  -> Either [CompileError] (Fragment Value Typed)
typeFragment prelude fragment
  = fst . runInference emptyEnv $ do
    typedFragment <- toTypedFragment fragment
    inferFragment prelude typedFragment

inferFragment
  :: Fragment Value Void
  -> Fragment Value Typed
  -> Inferred (Fragment Value Typed)
inferFragment prelude fragment = do

  forM_ (zip [0..] allDefs) $ \ (index, def)
    -> case manifestType (defTerm def) of
      Just scheme -> saveDef index scheme
      Nothing -> case find ((defName def ==) . defName) allDecls of
        Just decl -> saveDef index =<< fromAnno (defTerm decl)
        Nothing -> Inferred . throwMany . (:[]) . TypeError (defLocation def)
          $ "missing type declaration for '" ++ defName def ++ "'"

  forM_ (zip [(0 :: Int)..] allDefs) $ \ (index, def) -> do
    scheme <- generalize (inferValue (defTerm def))
    unifyM
      <$> (instantiateM =<< getsEnv ((! Name index) . envDefs))
      <*> instantiateM scheme

  void $ infer (Compose (fragmentTerms fragment))
  getsEnv (subFragment fragment)

  where

  allDecls = fragmentDecls prelude ++ fragmentDecls fragment
  allDefs = fragmentDefs prelude ++ fragmentDefs fragment

  saveDef :: Int -> Scheme -> Inferred ()
  saveDef index scheme = modifyEnv $ \ env -> env
    { envDefs = Map.insert (Name index) scheme (envDefs env) }


-- | Infers the type of a term.
infer :: Typed -> Inferred Type
infer typedTerm = case typedTerm of

  Builtin name loc -> withLocation loc $ case name of

    Builtin.AddVector
      -> (\ a -> [VectorType a, VectorType a] :> [VectorType a])
      <$> freshVarM

    Builtin.AddFloat -> binary FloatType

    Builtin.AddInt -> binary IntType

    Builtin.AndBool -> binary BoolType

    Builtin.AndInt -> binary IntType

    Builtin.Apply01
      -> (\ a -> [[] :> [a]] :> [a])
      <$> freshVarM

    Builtin.Apply10
      -> (\ a -> [a, [a] :> []] :> [])
      <$> freshVarM

    Builtin.Apply11
      -> (\ a b -> [a, [a] :> [b]] :> [b])
      <$> freshVarM
      <*> freshVarM

    Builtin.Apply21
      -> (\ a b c -> [a, b, [a, b] :> [c]] :> [c])
      <$> freshVarM
      <*> freshVarM
      <*> freshVarM

    Builtin.Bottom
      -> (\ a -> [VectorType a] :> [a])
      <$> freshVarM

    Builtin.Close -> return $ [HandleType] :> []

    Builtin.DecFloat -> unary FloatType

    Builtin.DecInt -> unary IntType

    Builtin.DivFloat -> binary FloatType

    Builtin.DivInt -> binary IntType

    Builtin.Down
      -> (\ a -> [VectorType a] :> [VectorType a])
      <$> freshVarM

    Builtin.Drop
      -> (\ a -> [a] :> [])
      <$> freshVarM

    Builtin.Dup
      -> (\ a -> [a] :> [a, a])
      <$> freshVarM

    Builtin.Empty
      -> (\ a -> [VectorType a] :> [BoolType])
      <$> freshVarM

    Builtin.EqVector -> freshVarM >>= \ a -> relational (VectorType a)
    Builtin.EqChar -> relational CharType
    Builtin.EqFloat -> relational FloatType
    Builtin.EqInt -> relational IntType

    Builtin.First
      -> (\ a b -> [a :* b] :> [a])
      <$> freshVarM <*> freshVarM

    Builtin.Function
      -> (\ a -> [a] :> [[] :> [a]])
      <$> freshVarM

    Builtin.GeVector -> freshVarM >>= \ a -> relational (VectorType a)
    Builtin.GeChar -> relational CharType
    Builtin.GeFloat -> relational FloatType
    Builtin.GeInt -> relational IntType

    Builtin.Get
      -> (\ a -> [VectorType a, IntType] :> [a])
      <$> freshVarM

    Builtin.GetLine
      -> return $ [HandleType] :> [VectorType CharType]

    Builtin.GtVector -> freshVarM >>= \ a -> relational (VectorType a)
    Builtin.GtChar -> relational CharType
    Builtin.GtFloat -> relational FloatType
    Builtin.GtInt -> relational IntType

    Builtin.IncFloat -> unary FloatType
    Builtin.IncInt -> unary IntType

    Builtin.LeVector -> freshVarM >>= \ a -> relational (VectorType a)
    Builtin.LeChar -> relational CharType
    Builtin.LeFloat -> relational FloatType
    Builtin.LeInt -> relational IntType

    Builtin.Length
      -> (\ a -> [VectorType a] :> [IntType])
      <$> freshVarM

    Builtin.LtVector -> freshVarM >>= \ a -> relational (VectorType a)
    Builtin.LtChar -> relational CharType
    Builtin.LtFloat -> relational FloatType
    Builtin.LtInt -> relational IntType

    Builtin.ModFloat -> binary FloatType
    Builtin.ModInt -> binary IntType

    Builtin.MulFloat -> binary FloatType
    Builtin.MulInt -> binary IntType

    Builtin.NeVector -> freshVarM >>= \ a -> relational (VectorType a)
    Builtin.NeChar -> relational CharType
    Builtin.NeFloat -> relational FloatType
    Builtin.NeInt -> relational IntType

    Builtin.NegFloat -> unary FloatType
    Builtin.NegInt -> unary IntType

    Builtin.NotBool -> unary BoolType
    Builtin.NotInt -> unary IntType

    Builtin.OpenIn
      -> return $ [VectorType CharType] :> [HandleType]

    Builtin.OpenOut
      -> return $ [VectorType CharType] :> [HandleType]

    Builtin.OrBool -> binary BoolType
    Builtin.OrInt -> binary IntType

    Builtin.Rest
      -> (\ a b -> [a :* b] :> [b])
      <$> freshVarM <*> freshVarM

    Builtin.Set
      -> (\ a -> [VectorType a, a, IntType] :> [VectorType a])
      <$> freshVarM

    Builtin.ShowFloat
      -> return $ [FloatType] :> [VectorType CharType]

    Builtin.ShowInt
      -> return $ [IntType] :> [VectorType CharType]

    Builtin.Stderr -> return $ [] :> [HandleType]
    Builtin.Stdin -> return $ [] :> [HandleType]
    Builtin.Stdout -> return $ [] :> [HandleType]

    Builtin.SubFloat -> binary FloatType
    Builtin.SubInt -> binary IntType

    Builtin.Print
      -> return $ [VectorType CharType, HandleType] :> []

    Builtin.Swap
      -> (\ a b -> [a, b] :> [b, a])
      <$> freshVarM <*> freshVarM

    Builtin.Top
      -> (\ a -> [VectorType a] :> [a])
      <$> freshVarM

    Builtin.Up
      -> (\ a -> [VectorType a] :> [VectorType a])
      <$> freshVarM

    Builtin.Vector
      -> (\ a -> [a] :> [VectorType a])
      <$> freshVarM

    Builtin.XorBool -> binary BoolType

    Builtin.XorInt -> binary IntType

  Call name loc -> withLocation loc
    $ instantiateM =<< getsEnv ((! name) . envDefs)

  Compose terms -> do
    types <- mapM infer terms
    foldM
      (\ (a :> b) (c :> d) -> inferCompose a b c d)
      ([] :> [])
      types

  If true false loc -> withLocation loc $ do
    (a :> b) <- infer true
    (c :> d) <- infer false
    unifyRowM a c
    unifyRowM b d
    return (a ++ [BoolType] :> b)

  Push value loc -> withLocation loc $ do
    a <- inferValue value
    return $ [] :> [a]

  Scoped term loc -> withLocation loc $ do
    a <- freshVarM
    (b :> c) <- local a $ infer term
    return $ b ++ [a] :> c

binary :: Type -> Inferred Type
binary a = return $ [a, a] :> [a]

relational :: Type -> Inferred Type
relational a = return $ [a, a] :> [BoolType]

unary :: Type -> Inferred Type
unary a = return $ [a] :> [a]

local :: Type -> Inferred a -> Inferred a
local type_ action = do
  modifyEnv $ \ env -> env { envLocals = type_ : envLocals env }
  result <- action
  modifyEnv $ \ env -> env { envLocals = tail $ envLocals env }
  return result

withClosure :: [Type] -> Inferred a -> Inferred a
withClosure types action = do
  original <- getsEnv envClosure
  modifyEnv $ \ env -> env { envClosure = types }
  result <- action
  modifyEnv $ \ env -> env { envClosure = original }
  return result

getClosedName :: ClosedName -> Inferred Type
getClosedName name = case name of
  ClosedName (Name index) -> getsEnv $ (!! index) . envLocals
  ReclosedName (Name index) -> getsEnv $ (!! index) . envClosure

manifestType :: Value -> Maybe Scheme
manifestType value = case value of
  Activation{} -> Nothing
  Bool{} -> Just $ mono BoolType
  Char{} -> Just $ mono CharType
  Closed{} -> Nothing
  Closure{} -> Nothing
  Escape{} -> Nothing
  Float{} -> Just $ mono FloatType
  Function{} -> Nothing
  Handle{} -> Just $ mono HandleType
  Int{} -> Just $ mono IntType
  Local{} -> Nothing
  Pair a b -> do
    Forall names1 type1 <- manifestType a
    Forall names2 type2 <- manifestType b
    return $ Forall (names1 <> names2) (type1 :* type2)
  Unit -> Just $ mono UnitType
  Vector{} -> Nothing

inferValue :: Value -> Inferred Type
inferValue value = case value of

  Activation{} -> error "TODO infer activation"

  Bool{} -> return BoolType

  Char{} -> return CharType

  Closed (Name index) -> getsEnv ((!! index) . envClosure)

  Closure names term -> do
    closed <- mapM getClosedName names
    withClosure closed (infer term)

  Escape name -> instantiateM =<< getsEnv ((! name) . envDefs)

  Float{} -> return FloatType

  Function term -> infer term

  Handle{} -> return HandleType
  Int{} -> return IntType

  Local (Name index) -> getsEnv ((!! index) . envLocals)

  Pair value1 value2 -> do
    a <- inferValue value1
    b <- inferValue value2
    return (a :* b)

  Unit -> return UnitType

  Vector values -> do
    valueTypes <- mapM inferValue values
    valueType <- unifyEach valueTypes
    return $ VectorType valueType

{-
checkAnno :: Maybe Scheme -> Type -> Inferred Type
checkAnno (Just scheme) type_ = unifyM type_ =<< instantiateM scheme
checkAnno Nothing type_ = return type_
-}

unifyEach :: [Type] -> Inferred Type
unifyEach (x : y : zs) = unifyM x y >> unifyEach (y : zs)
unifyEach [x] = return x
unifyEach [] = freshVarM

inferCompose :: [Type] -> [Type] -> [Type] -> [Type] -> Inferred Type
inferCompose in1 out1 in2 out2 = let
  (pairs, surplus, deficit) = zipRemainder (reverse out1) (reverse in2)
  consumption = reverse deficit ++ in1
  production = reverse surplus ++ out2
  in do
    mapM_ (uncurry unifyM_) pairs
    return $ consumption :> production

zipRemainder :: [a] -> [b] -> ([(a, b)], [a], [b])
zipRemainder = zipRemainder' []
  where
  zipRemainder' acc [] [] = (reverse acc, [], [])
  zipRemainder' acc xs [] = (reverse acc, xs, [])
  zipRemainder' acc [] ys = (reverse acc, [], ys)
  zipRemainder' acc (x : xs) (y : ys)
    = zipRemainder' ((x, y) : acc) xs ys
