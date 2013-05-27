{-# LANGUAGE GADTs #-}

module Kitten.Typecheck.Term
  ( typecheckTerm
  , typecheckTerms
  , typecheckValue
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Either
import Data.Set (Set)

import qualified Data.Set as Set

import Kitten.Anno (Anno)
import Kitten.Def
import Kitten.Name
import Kitten.Resolved
import Kitten.Type
import Kitten.Typecheck.Anno
import Kitten.Typecheck.Builtin
import Kitten.Typecheck.Manifest
import Kitten.Typecheck.Monad
import Kitten.Util.Function
import Kitten.Util.List
import Kitten.Util.Show

typecheckTerm :: Resolved -> Typecheck Resolved
typecheckTerm resolved = case resolved of

  Block terms -> Block <$> typecheckTerms terms

  Builtin builtin loc -> withLocation loc
    $ Builtin <$> typecheckBuiltin builtin <*> pure loc

  Closed{} -> internalError
    "closure variables should not appear during typechecking"

  If condition true false loc -> withLocation loc $ do
    condition' <- typecheckTerms condition
    popDataExpecting_ BoolType

    (true', afterTrue) <- hypothetically
      $ typecheckTerms true
    (false', afterFalse) <- hypothetically
      $ typecheckTerms false

    when (envData afterTrue /= envData afterFalse) $ let
      (afterTrueSuffix, afterFalseSuffix)
        = stripCommonPrefix
          (reverse $ envData afterTrue)
          (reverse $ envData afterFalse)
      in typeError $ unwords
       [ "incompatible types in 'then':"
       , showWords afterTrueSuffix
       , "and 'else':"
       , showWords afterFalseSuffix
       ]

    put afterTrue

    return $ If condition' true' false' loc

  Local name loc -> do
    withLocation loc $ pushData =<< getLocal name
    return resolved

  Push value loc -> do
    value' <- withLocation loc $ typecheckValue value
    return $ Push value' loc

  Scoped terms loc -> withLocation loc $ do
    pushLocal =<< popData
    Scoped <$> typecheckTerms terms <*> pure loc

typecheckTerms :: [Resolved] -> Typecheck [Resolved]
typecheckTerms = mapM typecheckTerm

typecheckTermShallow :: Resolved -> Typecheck Resolved
typecheckTermShallow resolved = case resolved of
  Push value loc -> Push <$> typecheckValueShallow value <*> pure loc
  _ -> typecheckTerm resolved

typecheckValueShallow :: Value -> Typecheck Value
typecheckValueShallow value = case value of
  Function anno _ -> do
    typecheckAnno anno
    return value
  Word names -> overload Word typecheckTermShallow names
  _ -> typecheckValue value

typecheckValue :: Value -> Typecheck Value
typecheckValue value = case value of

  Activation{} -> internalError
    "activations should not appear during typechecking"

  Bool{} -> do
    pushData BoolType
    return value

  Char{} -> do
    pushData CharType
    return value

  Closure anno _ _ -> do
    typecheckAnno anno
    return value

  Escape names -> overload Escape (pushData <=< manifestTermType) names

  Float{} -> do
    pushData FloatType
    return value

  Function anno terms -> do
    terms' <- typecheckWithAnno anno terms
    return $ Function anno terms'

  Handle{} -> do
    pushData HandleType
    return value

  Int{} -> do
    pushData IntType
    return value

  Pair a b -> do
    void $ typecheckValue b
    void $ typecheckValue a
    aType <- popData
    bType <- popData
    pushData $ PairType aType bType
    return value

  Unit -> do
    pushData UnitType
    return value

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
          void $ typecheckValue element
          actual <- popData
          when (actual /= annotated) . typeError $ unwords
            [ "element type"
            , show actual
            , "does not match annotated type"
            , show annotated
            ]
      Nothing -> return ()

    pushData $ VectorType expected
    return value

  Word names -> overload Word typecheckTermShallow names

overload
  :: (Set Name -> Value)
  -> (Resolved -> Typecheck a)
  -> Set Name
  -> Typecheck Value
overload wrap action names = do

  defTable <- gets envDefs
  let
    indexedDefs = for indices $ \ index
      -> (index, defTable !! index)

  env <- get
  let
    results = for indexedDefs $ \ (index, def)
      -> flip runStateT env $ do
        void . action $ defTerm def
        return index

  case partitionEithers results of
    (_, [(index, result)]) -> do
      put result
      return . wrap . Set.singleton $ Name index

    (_, []) -> typeError "unable to resolve overload"
    (_, _) -> typeError "ambiguous overload"

  where indices = map nameIndex $ Set.toList names

typecheckWithAnno
  :: Anno
  -> [Resolved]
  -> Typecheck [Resolved]
typecheckWithAnno anno terms = do
  (terms', _) <- hypothetically $ case type_ of
    AnyType -> unknownTypeError
    AnyType :> AnyType -> unknownTypeError
    AnyType :> Composition{} -> unknownTypeError
    BoolType -> simple
    CharType -> simple
    Composition consumption :> Composition production -> do
      pushData StackFrameType
      mapM_ pushData consumption
      result <- typecheckTerms terms
      mapM_ popDataExpecting_ $ reverse production
      popDataExpecting_ StackFrameType
      return result
    Composition{} :> AnyType -> unknownTypeError
    FloatType -> simple
    HandleType -> simple
    IntType -> simple
    PairType{} -> simple
    StackFrameType -> internalError
      "stack frames should not appear in annotations"
    UnitType -> simple
    VectorType{} -> simple

  pushData type_
  return terms'

  where

  type_ :: Type Scalar
  type_ = fromAnno anno

  simple :: Typecheck [Resolved]
  simple = do
    pushData type_
    return terms
