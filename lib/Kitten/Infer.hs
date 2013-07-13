module Kitten.Infer
  ( infer
  , typeFragment
  ) where

import Control.Applicative
import Control.Monad
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
import Kitten.Location
import Kitten.Name
import Kitten.Purity
import Kitten.Type
import Kitten.Resolved
import Kitten.Util.FailWriter
import Kitten.Util.Show
import Kitten.Util.Void

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Resolved as Resolved

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

  forM_ (zip [(0 :: Int)..] allDefs) $ \ (index, def) -> do

    scheme <- generalize (inferValue (defTerm def))
    declared <- fmap normalize . instantiateM
      =<< getsEnv ((! Name index) . envDefs)
    inferred <- normalize <$> instantiateM scheme

    unless (inferred <: declared)
      $ Inferred . throwMany . (:[])
      . TypeError (defLocation def) $ unwords
      [ "declared type"
      , show declared
      , "does not match inferred type"
      , show inferred
      , "of"
      , defName def
      ]

  FunctionType consumption _ _
    <- infer (Compose (fragmentTerms fragment))

  unless (null consumption)
    . Inferred . throwMany . (:[]) . TypeError UnknownLocation $ unwords
    [ "program must work on empty stack but expects"
    , showWords consumption
    ]

  where

  allDefs = fragmentDefs prelude ++ fragmentDefs fragment

  saveDef :: Int -> Scheme -> Inferred ()
  saveDef index scheme = modifyEnv $ \ env -> env
    { envDefs = Map.insert (Name index) scheme (envDefs env) }


-- | Infers the type of a term.
infer :: Resolved -> Inferred Type
infer typedTerm = case typedTerm of

  Builtin name loc -> withLocation loc $ case name of

    Builtin.AddVector
      -> (\ a -> [VectorType a, VectorType a] --> [VectorType a])
      <$> freshVarM

    Builtin.AddFloat -> binary FloatType

    Builtin.AddInt -> binary IntType

    Builtin.AndBool -> binary BoolType

    Builtin.AndInt -> binary IntType

    Builtin.Apply01
      -> (\ a -> [[] --> [a]] --> [a])
      <$> freshVarM

    Builtin.Apply11
      -> (\ a b -> [a, [a] --> [b]] --> [b])
      <$> freshVarM
      <*> freshVarM

    Builtin.Apply21
      -> (\ a b c -> [a, b, [a, b] --> [c]] --> [c])
      <$> freshVarM
      <*> freshVarM
      <*> freshVarM

    Builtin.Call01
      -> (\ a -> [[] ==> [a]] ==> [a])
      <$> freshVarM

    Builtin.Call10
      -> (\ a -> [a, [a] ==> []] ==> [])
      <$> freshVarM

    Builtin.CharToInt -> return $ [CharType] --> [IntType]

    Builtin.Close -> return $ [HandleType] ==> []

    Builtin.DivFloat -> binary FloatType

    Builtin.DivInt -> binary IntType

    Builtin.EqFloat -> relational FloatType
    Builtin.EqInt -> relational IntType

    Builtin.Exit -> return $ [IntType] ==> []

    Builtin.First
      -> (\ a b -> [a :& b] --> [a])
      <$> freshVarM <*> freshVarM

    Builtin.GeFloat -> relational FloatType
    Builtin.GeInt -> relational IntType

    Builtin.Get
      -> (\ a -> [VectorType a, IntType] --> [a])
      <$> freshVarM

    Builtin.GetLine
      -> return $ [HandleType] ==> [VectorType CharType]

    Builtin.GtFloat -> relational FloatType
    Builtin.GtInt -> relational IntType

    Builtin.Impure -> return $ [] ==> []

    Builtin.Init
      -> (\ a -> [VectorType a] --> [VectorType a])
      <$> freshVarM

    Builtin.LeFloat -> relational FloatType
    Builtin.LeInt -> relational IntType

    Builtin.Length
      -> (\ a -> [VectorType a] --> [IntType])
      <$> freshVarM

    Builtin.LtFloat -> relational FloatType
    Builtin.LtInt -> relational IntType

    Builtin.ModFloat -> binary FloatType
    Builtin.ModInt -> binary IntType

    Builtin.MulFloat -> binary FloatType
    Builtin.MulInt -> binary IntType

    Builtin.NeFloat -> relational FloatType
    Builtin.NeInt -> relational IntType

    Builtin.NegFloat -> unary FloatType
    Builtin.NegInt -> unary IntType

    Builtin.NotBool -> unary BoolType
    Builtin.NotInt -> unary IntType

    Builtin.OpenIn
      -> return $ [VectorType CharType] ==> [HandleType]

    Builtin.OpenOut
      -> return $ [VectorType CharType] ==> [HandleType]

    Builtin.OrBool -> binary BoolType
    Builtin.OrInt -> binary IntType

    Builtin.Rest
      -> (\ a b -> [a :& b] --> [b])
      <$> freshVarM <*> freshVarM

    Builtin.Set
      -> (\ a -> [VectorType a, a, IntType] --> [VectorType a])
      <$> freshVarM

    Builtin.ShowFloat
      -> return $ [FloatType] --> [VectorType CharType]

    Builtin.ShowInt
      -> return $ [IntType] --> [VectorType CharType]

    Builtin.Stderr -> return $ [] --> [HandleType]
    Builtin.Stdin -> return $ [] --> [HandleType]
    Builtin.Stdout -> return $ [] --> [HandleType]

    Builtin.SubFloat -> binary FloatType
    Builtin.SubInt -> binary IntType

    Builtin.Pair
      -> (\ a b -> [a, b] --> [a :& b])
      <$> freshVarM <*> freshVarM

    Builtin.Print
      -> return $ [VectorType CharType, HandleType] ==> []

    Builtin.Tail
      -> (\ a -> [VectorType a] --> [VectorType a])
      <$> freshVarM

    Builtin.UnsafePurify11
      -> (\ a b -> [[a] ==> [b]] --> [[a] --> [b]])
      <$> freshVarM <*> freshVarM

    Builtin.Vector
      -> (\ a -> [a] --> [VectorType a])
      <$> freshVarM

    Builtin.XorBool -> binary BoolType

    Builtin.XorInt -> binary IntType

  Call name loc -> withLocation loc
    $ instantiateM =<< getsEnv ((! name) . envDefs)

  Compose terms -> do
    types <- mapM infer terms
    foldM
      (\ (FunctionType a b p1) (FunctionType c d p2)
        -> inferCompose a b c d (p1 <=> p2))
      ([] --> [])
      types

  If true false loc -> withLocation loc $ do
    FunctionType a b p1 <- infer true
    FunctionType c d p2 <- infer false
    unifyRowM a c
    unifyRowM b d
    return (FunctionType (a ++ [BoolType]) b (p1 <=> p2))

  PairTerm a b loc -> withLocation loc $ do
    a' <- fromConstant =<< infer a
    b' <- fromConstant =<< infer b
    return $ [] --> [a' :& b']

  Push value loc -> withLocation loc $ do
    a <- inferValue value
    return $ [] --> [a]

  Scoped term loc -> withLocation loc $ do
    a <- freshVarM
    FunctionType b c p <- local a $ infer term
    return $ FunctionType (b ++ [a]) c p

  VectorTerm values loc -> withLocation loc $ do
    values' <- mapM infer values
    values'' <- fromConstant =<< unifyEach values'
    return $ FunctionType [] [VectorType values'']
      (if any isImpure values' then Impure else Pure)

fromConstant :: Type -> Inferred Type
fromConstant (FunctionType [] [a] _) = return a
fromConstant (FunctionType [] xs _) = do
  here <- getsEnv envLocation
  Inferred . throwMany . (:[]) . TypeError here $ unwords
    [ "expected one value but got"
    , show (length xs)
    ]
fromConstant a = return a

binary :: Type -> Inferred Type
binary a = return $ [a, a] --> [a]

relational :: Type -> Inferred Type
relational a = return $ [a, a] --> [BoolType]

unary :: Type -> Inferred Type
unary a = return $ [a] --> [a]

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
  Float{} -> Just $ mono FloatType
  Function{} -> Nothing
  Handle{} -> Just $ mono HandleType
  Int{} -> Just $ mono IntType
  Local{} -> Nothing
  Pair a b -> do
    Forall names1 type1 <- manifestType a
    Forall names2 type2 <- manifestType b
    return $ Forall (names1 <> names2) (type1 :& type2)
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

  Float{} -> return FloatType

  Function term -> infer term

  Handle{} -> return HandleType
  Int{} -> return IntType

  Local (Name index) -> getsEnv ((!! index) . envLocals)

  Pair a b -> (:&) <$> inferValue a <*> inferValue b

  Unit -> return UnitType

  Vector values -> do
    valueTypes <- mapM inferValue values
    valueType <- unifyEach valueTypes
    return $ VectorType valueType

unifyEach :: [Type] -> Inferred Type
unifyEach (x : y : zs) = unifyM x y >> unifyEach (y : zs)
unifyEach [x] = return x
unifyEach [] = freshVarM

inferCompose
  :: [Type]
  -> [Type]
  -> [Type]
  -> [Type]
  -> Purity
  -> Inferred Type
inferCompose in1 out1 in2 out2 purity = let
  (pairs, surplus, deficit) = zipRemainder (reverse out1) (reverse in2)
  consumption = reverse deficit ++ in1
  production = reverse surplus ++ out2
  in do
    mapM_ (uncurry unifyM_) pairs
    return $ FunctionType consumption production purity

zipRemainder :: [a] -> [b] -> ([(a, b)], [a], [b])
zipRemainder = zipRemainder' []
  where
  zipRemainder' acc [] [] = (reverse acc, [], [])
  zipRemainder' acc xs [] = (reverse acc, xs, [])
  zipRemainder' acc [] ys = (reverse acc, [], ys)
  zipRemainder' acc (x : xs) (y : ys)
    = zipRemainder' ((x, y) : acc) xs ys
