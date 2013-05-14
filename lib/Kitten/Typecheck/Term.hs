{-# LANGUAGE RecordWildCards #-}

module Kitten.Typecheck.Term
  ( typecheckTerm
  , typecheckTerms
  , typecheckValue
  ) where

import Control.Monad
import Control.Monad.Trans.State

import Kitten.Def
import Kitten.Name
import Kitten.Resolved
import Kitten.Type
import Kitten.Typecheck.Anno
import Kitten.Typecheck.Builtin
import Kitten.Typecheck.Manifest
import Kitten.Typecheck.Monad
import Kitten.Util.List
import Kitten.Util.Show

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
       , showWords afterTrueSuffix
       , "and 'else':"
       , showWords afterFalseSuffix
       ]

    put afterTrue

  Local name loc -> withLocation loc
    $ pushData =<< getLocal name

  Push value loc -> withLocation loc $ typecheckValue value

  Scoped terms loc -> withLocation loc $ do
    pushLocal =<< popData
    typecheckTerms terms

typecheckTerms :: [Resolved] -> Typecheck
typecheckTerms = mapM_ typecheckTerm

typecheckTermShallow :: Resolved -> Typecheck
typecheckTermShallow resolved = case resolved of
  Push value _ -> typecheckValueShallow value
  _ -> typecheckTerm resolved

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

  Bool{} -> pushData BoolType

  Char{} -> pushData CharType

  Closure anno _ _ -> typecheckAnno anno

  Escape (Name index) -> do
    mDef <- gets $ (!? index) . envDefs
    case mDef of
      Nothing -> internalError
        "unresolved name appeared during type inference"
      Just Def{..} -> pushData =<< manifestTermType defTerm

  Float{} -> pushData FloatType

  Function anno terms -> typecheckAnnotatedTerms anno
    $ typecheckTerms terms

  Handle{} -> pushData HandleType

  Int{} -> pushData IntType

  Pair a b -> do
    typecheckValue b
    typecheckValue a
    aType <- popData
    bType <- popData
    pushData $ PairType aType bType

  Unit -> pushData $ UnitType

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
