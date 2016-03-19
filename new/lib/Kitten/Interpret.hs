{-# LANGUAGE OverloadedStrings #-}

module Kitten.Interpret
  ( interpret
  ) where

import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Kitten.Definition (mainName)
import Kitten.Dictionary (Dictionary)
import Kitten.Name
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Type(..))
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Term as Term
import qualified Kitten.Vocabulary as Vocabulary

interpret
  :: Dictionary
  -> Maybe Qualified
  -> [Value Type]
  -> IO [Value Type]
interpret dictionary mName initialStack = do
  -- TODO: Types.
  stackRef <- newIORef initialStack
  localsRef <- newIORef []
  currentClosureRef <- newIORef []
  let

    word :: Qualified -> IO ()
    word name = case Dictionary.lookup name dictionary of
      Just (Entry.Word _ _ _ _ _ Nothing) -> case name of
        Qualified v unqualified
          | v == Vocabulary.global
          -> case unqualified of
            "empty" -> do
              (Array xs : r) <- readIORef stackRef
              writeIORef stackRef r
              case xs of
                [] -> word $ Qualified Vocabulary.global "true"
                _ : _ -> word $ Qualified Vocabulary.global "false"
            "head" -> do
              (Array xs : r) <- readIORef stackRef
              case xs of
                [] -> do
                  writeIORef stackRef r
                  word $ Qualified Vocabulary.global "none"
                x : _ -> do
                  writeIORef stackRef $ x : r
                  word $ Qualified Vocabulary.global "some"
            "prepend" -> do
              (Array xs : x : r) <- readIORef stackRef
              writeIORef stackRef $ Array (x : xs) : r
            "tail" -> do
              (Array xs : r) <- readIORef stackRef
              case xs of
                [] -> do
                  writeIORef stackRef r
                  word $ Qualified Vocabulary.global "none"
                _ : x -> do
                  writeIORef stackRef $ Array x : r
                  word $ Qualified Vocabulary.global "some"
            _ -> error "no such intrinsic"
        _ -> error "no such intrinsic"
      Just (Entry.Word _ _ _ _ _ (Just body)) -> term body
      _ -> error "not a word about this"

    term :: Term Type -> IO ()
    term t = case t of
      Call _ _ -> call
      Compose _ a b -> term a >> term b
      Drop _ _ -> modifyIORef' stackRef tail
      Generic _ _ _ -> error "generic"
      Group t' -> term t'
      Identity _ _ -> return ()
      If _ true false _ -> do
        (Algebraic (ConstructorIndex index) [] : r) <- readIORef stackRef
        writeIORef stackRef r
        term $ case toEnum index of
          False -> false
          True -> true
      Lambda _ _name _ body _ -> do
        (a : r) <- readIORef stackRef
        ls <- readIORef localsRef
        writeIORef stackRef r
        writeIORef localsRef (a : ls)
        term body
        modifyIORef' localsRef tail
      Match _ cases mElse _ -> do
        -- We delay matching on the value here because it may not be an ADT at
        -- all. For example, "1 match { else { 2 } }" is perfectly valid,
        -- because we are matching on all (0) of Int32's constructors.
        (x : r) <- readIORef stackRef
        writeIORef stackRef r
        let
          go (Case (QualifiedName name) caseBody _ : _)
            -- FIXME: Embed this information during name resolution, rather than
            -- looking it up.
            | Just (Entry.Word _ _ _ _ _ (Just ctorBody))
              <- Dictionary.lookup name dictionary
            , [New _ (ConstructorIndex index') _ _] <- Term.decompose ctorBody
            , Algebraic (ConstructorIndex index) fields <- x
            , index == index'
            = do
              writeIORef stackRef (fields ++ r)
              term caseBody
          go (_ : rest) = go rest
          go [] = case mElse of
            Just (Else body _) -> term body
            Nothing -> error "pattern match failure"
        go cases
      New _ index size _ -> do
        r <- readIORef stackRef
        let (fields, r') = splitAt size r
        writeIORef stackRef $ Algebraic index fields : r'
      NewClosure _ size _ -> do
        r <- readIORef stackRef
        let (Name name : closure, r') = splitAt (size + 1) r
        writeIORef stackRef (Closure name closure : r')
      NewVector _ size _ -> do
        r <- readIORef stackRef
        let (values, r') = splitAt size r
        writeIORef stackRef (Array (reverse values) : r')
      Push _ value _ -> push value
      Swap _ _ -> do
        (a : b : r) <- readIORef stackRef
        writeIORef stackRef (b : a : r)
      With _ _ _ -> call
      Word _ _ (QualifiedName name) _args _ -> word name
      Word _ _ name _ _ -> error "unresolved word name"

    call :: IO ()
    call = do
      (Closure name closure : r) <- readIORef stackRef
      writeIORef stackRef r
      modifyIORef' currentClosureRef (closure :)
      word name
      modifyIORef' currentClosureRef tail

    push :: Value Type -> IO ()
    push value = case value of
      Closed (ClosureIndex index) -> do
        (currentClosure : _) <- readIORef currentClosureRef
        modifyIORef' stackRef ((currentClosure !! index) :)
      Local (LocalIndex index) -> do
        locals <- readIORef localsRef
        modifyIORef' stackRef ((locals !! index) :)
      _ -> modifyIORef' stackRef (value :)

  word $ fromMaybe mainName mName
  readIORef stackRef
