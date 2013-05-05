{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.Typecheck
  ( typecheck
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Kitten.Anno (Anno(..))
import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Location
import Kitten.Name
import Kitten.Resolve (Resolved(..), Value(..))
import Kitten.Type
import Kitten.Util.List

import qualified Kitten.Builtin as Builtin

data Env = Env
  { envData :: [Type Scalar]
  , envLocals :: [Type Scalar]
  , envDefs :: [Def Resolved]
  , envLocations :: [Location]
  }

type TypecheckM a = StateT Env (Either CompileError) a
type Typecheck = TypecheckM ()

typecheck
  :: [Def Resolved]
  -> [Value]
  -> Fragment Resolved
  -> Either CompileError ()
typecheck _prelude stack Fragment{..}
  = flip evalStateT emptyEnv
  { envDefs = fragmentDefs
  } $ do
    mapM_ typecheckValue stack
    mapM_ typecheckDef fragmentDefs
    typecheckTerms fragmentTerms

typecheckTerms :: [Resolved] -> Typecheck
typecheckTerms = mapM_ typecheckTerm

typecheckDef :: Def Resolved -> Typecheck
typecheckDef Def{..} = withLocation defLocation
  . void . hypothetically $ typecheckTerm defTerm

emptyEnv :: Env
emptyEnv = Env
  { envData = []
  , envLocals = []
  , envDefs = []
  , envLocations = []
  }

typecheckTermShallow :: Resolved -> Typecheck
typecheckTermShallow resolved = case resolved of
  Push value loc -> withLocation loc
    $ typecheckValueShallow value
  _ -> typecheckTerm resolved

typecheckTerm :: Resolved -> Typecheck
typecheckTerm resolved = case resolved of

  Block terms -> typecheckTerms terms

  Builtin builtin loc -> withLocation loc
    $ typecheckBuiltin builtin

  Closed{} -> internalError
    "closure variables should not appear during typechecking"

  If condition true false loc -> withLocation loc $ do
    typecheckTerms condition
    popDataExpecting_ BoolType

    (_, afterTrue) <- hypothetically
      $ typecheckTerms true
    (_, afterFalse) <- hypothetically
      $ typecheckTerms false

    when (envData afterTrue /= envData afterFalse) $ let
      (afterTrueSuffix, afterFalseSuffix)
        = stripCommonPrefix
          (reverse $ envData afterTrue)
          (reverse $ envData afterFalse)
      in typeError $ unwords
       [ "incompatible types in 'then':"
       , unwords $ map show afterTrueSuffix
       , "and 'else':"
       , unwords $ map show afterFalseSuffix
       ]

    put afterTrue

  Local name loc -> withLocation loc
    $ pushData =<< getLocal name

  Push value loc -> withLocation loc $ typecheckValue value

  Scoped terms loc -> withLocation loc $ do
    pushLocal =<< popData
    typecheckTerms terms

manifestTermType :: Resolved -> TypecheckM (Type Scalar)
manifestTermType resolved = case resolved of
  Push value _ -> manifestValueType value
  _ -> internalError "TODO manifest term type of non-function"

manifestValueType :: Value -> TypecheckM (Type Scalar)
manifestValueType value = case value of
  Bool _ -> return BoolType
  Function anno _ -> return $ fromAnno anno
  Int _ -> return IntType
  Text _ -> return TextType
  Vector (Just anno) _ -> return . VectorType $ fromAnno anno
  _ -> internalError "TODO manifest value type of non-literal"

typecheckValueShallow :: Value -> Typecheck
typecheckValueShallow value = case value of
  Function anno _ -> typecheckAnno anno
  Word (Name index) -> do
    mDef <- gets $ (!? index) . envDefs
    case mDef of
      Nothing -> internalError
        "unresolved name appeared during type inference"
      Just Def{..} -> typecheckTermShallow defTerm
  _ -> typecheckValue value

typecheckValue :: Value -> Typecheck
typecheckValue value = case value of

  Activation{} -> internalError
    "activations should not appear during typechecking"

  Bool _ -> pushData BoolType

  Closure{} -> internalError
    "closures should not appear during typechecking"

  Escape (Name index) -> do
    mDef <- gets $ (!? index) . envDefs
    case mDef of
      Nothing -> internalError
        "unresolved name appeared during type inference"
      Just Def{..} -> pushData =<< manifestTermType defTerm

  Function anno terms -> typecheckAnnotatedTerms anno terms

  Int _ -> pushData IntType

  Text _ -> pushData TextType

  Vector mAnno elements -> do

    expected <- case elements of
      _ : _ -> do
        mapM_ typecheckValue elements
        (expected : rest) <- replicateM (length elements) popData
        forM_ rest $ \ actual -> when (actual /= expected)
          . typeError $ unwords
          [ "element type"
          , show actual
          , "does not match first element type"
          , show expected
          ]
        return expected

      [] -> case mAnno of
        Nothing -> typeError
          "empty vectors require type annotations"
        Just anno -> return $ fromAnno anno

    case mAnno of
      Just anno -> do
        let annotated = fromAnno anno
        forM_ elements $ \ element -> do
          typecheckValue element
          actual <- popData
          when (actual /= annotated) . typeError $ unwords
            [ "element type"
            , show actual
            , "does not match annotated type"
            , show annotated
            ]
      Nothing -> return ()

    pushData $ VectorType expected

  Word (Name index) -> do
    mDef <- gets $ (!? index) . envDefs
    case mDef of
      Nothing -> internalError
        "unresolved name appeared during type inference"
      Just Def{..} -> typecheckTermShallow defTerm

stackEffect :: TypecheckM a -> TypecheckM (Type Scalar)
stackEffect action = do
  (before, after) <- hypothetically action
  let
    stackBefore = envData before
    stackAfter = envData after
    (consumption, production) = stripCommonPrefix
      (reverse stackBefore)
      (reverse stackAfter)
  return
    $ Composition (reverse consumption)
    :> Composition (reverse production)

typecheckAnnotatedTerms :: Anno -> [Resolved] -> Typecheck
typecheckAnnotatedTerms anno terms = do
  void . hypothetically $ case type_ of
    BoolType -> pushData type_
    IntType -> pushData type_
    TextType -> pushData type_
    VectorType _ -> pushData type_
    Composition consumption :> Composition production -> do
      pushData StackFrameType
      mapM_ pushData consumption
      typecheckTerms terms
      mapM_ popDataExpecting_ $ reverse production
      popDataExpecting_ StackFrameType
    StackFrameType -> internalError
      "stack frames should not appear in annotations"
    AnyType :> Composition _ -> unknownTypeError
    Composition _ :> AnyType -> unknownTypeError
    AnyType :> AnyType -> unknownTypeError
    AnyType -> unknownTypeError

  pushData type_

  where
  type_ :: Type Scalar
  type_ = fromAnno anno

unknownTypeError :: Typecheck
unknownTypeError = internalError
  "unknown type appeared during typechecking"

typecheckAnno :: Anno -> Typecheck
typecheckAnno anno = go $ fromAnno anno
  where
  go :: Type Scalar -> Typecheck
  go type_ = case type_ of
    BoolType -> pushData type_
    IntType -> pushData type_
    TextType -> pushData type_
    VectorType _ -> pushData type_
    Composition consumption :> Composition production -> do
      mapM_ popDataExpecting_ $ reverse consumption
      mapM_ pushData production
    AnyType :> Composition _ -> unknownTypeError
    Composition _ :> AnyType -> unknownTypeError
    AnyType :> AnyType -> unknownTypeError
    AnyType -> unknownTypeError
    StackFrameType -> internalError
      "stack frames should not appear in annotations"

typecheckBuiltin :: Builtin -> Typecheck
typecheckBuiltin builtin = case builtin of
  Builtin.Add -> intsToInt

  Builtin.AndBool -> boolsToBool

  Builtin.AndInt -> intsToInt

  Builtin.Apply -> do
    Composition consumption :> Composition production
      <- popDataExpecting $ AnyType :> AnyType
    mapM_ popDataExpecting_ $ reverse consumption
    mapM_ pushData production

  Builtin.At -> do
    popDataExpecting_ IntType
    VectorType a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Bottom -> do
    VectorType a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Cat -> do
    VectorType b <- popDataExpecting $ VectorType AnyType
    VectorType a <- popDataExpecting $ VectorType AnyType
    if a == b
      then pushData $ VectorType a
      else typeError $ concat
        [ "mismatched element types "
        , show a
        , " and "
        , show b
        , " in 'cat'"
        ]

  Builtin.Compose -> do
    Composition c :> Composition d
      <- popDataExpecting $ AnyType :> AnyType
    Composition a :> Composition b
      <- popDataExpecting $ AnyType :> AnyType
    result <- stackEffect $ do
      mapM_ popDataExpecting a
      mapM_ pushData b
      mapM_ popDataExpecting c
      mapM_ pushData d
    pushData result

  Builtin.Div -> intsToInt

  Builtin.Down -> do
    a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Drop -> popData_

  Builtin.Dup -> do
    a <- popData
    pushData a
    pushData a

  Builtin.Eq -> intsToBool

  Builtin.Empty -> do
    popDataExpecting_ $ VectorType AnyType
    pushData BoolType

  Builtin.Function -> do
    a <- popData
    pushData $ Composition [] :> Composition [a]

  Builtin.Ge -> intsToBool

  Builtin.Gt -> intsToBool

  Builtin.Le -> intsToBool

  Builtin.Length -> do
    popDataExpecting_ $ VectorType AnyType
    pushData IntType

  Builtin.Lt -> intsToBool

  Builtin.Mod -> intsToInt

  Builtin.Mul -> intsToInt

  Builtin.Ne -> intsToBool

  Builtin.Neg -> intToInt

  Builtin.NotBool -> boolToBool

  Builtin.NotInt -> intToInt

  Builtin.OrBool -> boolsToBool

  Builtin.OrInt -> intsToInt

  Builtin.Print -> popDataExpecting_ TextType

  Builtin.ShowInt -> do
    popDataExpecting_ IntType
    pushData TextType

  Builtin.Sub -> intsToInt

  Builtin.Swap -> do
    a <- popData
    b <- popData
    pushData a
    pushData b

  Builtin.Top -> do
    VectorType a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Up -> do
    a <- popDataExpecting $ VectorType AnyType
    pushData a

  Builtin.Vector -> do
    a <- popData
    pushData $ VectorType a

  Builtin.XorBool -> boolsToBool

  Builtin.XorInt -> intsToInt

  where

  boolToBool = do
    popDataExpecting_ BoolType
    pushData BoolType

  boolsToBool = do
    popDataExpecting_ BoolType
    popDataExpecting_ BoolType
    pushData BoolType

  intToInt = do
    popDataExpecting_ IntType
    pushData IntType

  intsToInt = do
    popDataExpecting_ IntType
    popDataExpecting_ IntType
    pushData IntType

  intsToBool = do
    popDataExpecting_ IntType
    popDataExpecting_ IntType
    pushData BoolType

typeError :: String -> TypecheckM a
typeError message = do
  location <- here
  lift . Left $ TypeError location message

internalError :: String -> TypecheckM a
internalError = lift . Left . InternalError

popData_ :: Typecheck
popData_ = void popData

popData :: TypecheckM (Type Scalar)
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> typeError "unexpected empty stack"
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      return top

popDataExpecting_ :: Type Scalar -> Typecheck
popDataExpecting_ = void . popDataExpecting

popDataExpecting :: Type Scalar -> TypecheckM (Type Scalar)
popDataExpecting type_ = do
  dataStack <- gets envData
  case dataStack of
    [] -> typeError $ unwords
      [ "stack underflow does not match expected type"
      , show type_
      ]
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      unify top type_
      return top

pushData :: Type Scalar -> Typecheck
pushData type_ = modify $ \ env@Env{..}
  -> env { envData = type_ : envData }

getLocal :: Name -> TypecheckM (Type Scalar)
getLocal (Name index) = gets ((!! index) . envLocals)

pushLocal :: Type Scalar -> Typecheck
pushLocal type_ = modify $ \ env@Env{..}
  -> env { envLocals = type_ : envLocals }

hypothetically :: TypecheckM a -> TypecheckM (Env, Env)
hypothetically action = do
  before <- get
  void action
  after <- get
  put before
  return (before, after)

unify
  :: Type Scalar
  -> Type Scalar
  -> Typecheck
unify actual expected
  = when (actual /= expected) . typeError $ unwords
  [ "expected"
  , show expected
  , "but got"
  , show actual
  ]

here :: TypecheckM Location
here = do
  locations <- gets envLocations
  return $ case locations of
    [] -> UnknownLocation
    (location : _) -> location

withLocation :: Location -> TypecheckM a -> TypecheckM a
withLocation location action = do
  modify $ \ env@Env{..} -> env
    { envLocations = location : envLocations }
  result <- action
  modify $ \ env@Env{..} -> env
    { envLocations = tail envLocations }
  return result
