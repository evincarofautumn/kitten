{-# LANGUAGE OverloadedStrings #-}

module Kitten.Interpret
  ( Failure
  , interpret
  ) where

import Control.Exception (Exception, throwIO)
import Data.Fixed (mod')
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word
import Kitten.Definition (mainName)
import Kitten.Dictionary (Dictionary)
import Kitten.Monad (runKitten)
import Kitten.Name
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Type(..))
import qualified Data.Text as Text
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Mangle as Mangle
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Report as Report
import qualified Kitten.Term as Term
import qualified Kitten.TypeEnv as TypeEnv
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

interpret
  :: Dictionary
  -> Maybe Qualified
  -> [Type]
  -> [Value Type]
  -> IO [Value Type]
interpret dictionary mName mainArgs initialStack = do
  -- TODO: Types.
  stackRef <- newIORef initialStack
  localsRef <- newIORef []
  currentClosureRef <- newIORef []
  let

    word :: Qualified -> [Type] -> IO ()
    word name args = do
      let
        mangled = Qualified Vocabulary.global
          $ Unqualified $ Mangle.name name args
      case Dictionary.lookup mangled dictionary of
        -- An entry in the dictionary should already be instantiated, so we
        -- shouldn't need to instantiate it again here.
        Just (Entry.Word _ _ _ _ _ (Just body)) -> term body
        _ -> case Dictionary.lookup name dictionary of
          -- A regular word.
          Just (Entry.Word _ _ _ _ _ (Just body)) -> do
            mBody' <- runKitten $ Instantiate.term TypeEnv.empty body args
            case mBody' of
              Right body' -> term body'
              Left reports -> do
                putStrLn $ Pretty.render $ Pretty.vcat
                  $ Pretty.hcat
                    [ "Could not instantiate generic word "
                    , Pretty.quote name
                    , ":"
                    ]
                  : map Report.human reports
          -- An intrinsic.
          Just (Entry.Word _ _ _ _ _ Nothing) -> case name of
            Qualified v unqualified
              | v == Vocabulary.intrinsic
              -> intrinsic unqualified
            _ -> error "no such intrinsic"
          _ -> do
            throwIO $ Failure $ Pretty.hcat
              [ "I can't find an instantiation of "
              , Pretty.quote name
              , ": "
              , Pretty.quote mangled
              ]
    term :: Term Type -> IO ()
    term t = case t of
      Call _ _ -> call
      Compose _ a b -> term a >> term b
      Drop _ _ -> modifyIORef' stackRef tail
      -- TODO: Verify that this is correct.
      Generic _ t' _ -> term t'
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
      Word _ _ (QualifiedName name) args _ -> word name args
      Word _ _ name _ _ -> error "unresolved word name"

    call :: IO ()
    call = do
      (Closure name closure : r) <- readIORef stackRef
      writeIORef stackRef r
      modifyIORef' currentClosureRef (closure :)
      -- FIXME: Use right args.
      word name []
      modifyIORef' currentClosureRef tail

    push :: Value Type -> IO ()
    push value = case value of
      Closed (ClosureIndex index) -> do
        (currentClosure : _) <- readIORef currentClosureRef
        modifyIORef' stackRef ((currentClosure !! index) :)
      Local (LocalIndex index) -> do
        locals <- readIORef localsRef
        modifyIORef' stackRef ((locals !! index) :)
      Text text -> modifyIORef' stackRef
        ((Array $ map Character $ Text.unpack text) :)
      _ -> modifyIORef' stackRef (value :)

    intrinsic :: Unqualified -> IO ()
    intrinsic name = case name of
      "add_int32" -> binaryInt32 (+)
      "sub_int32" -> binaryInt32 (-)
      "mul_int32" -> binaryInt32 (*)
      "div_int32" -> binaryInt32 div
      "mod_int32" -> binaryInt32 mod
      "add_float64" -> binaryFloat64 (+)
      "sub_float64" -> binaryFloat64 (-)
      "mul_float64" -> binaryFloat64 (*)
      "div_float64" -> binaryFloat64 (/)
      "mod_float64" -> binaryFloat64 mod'
      "empty" -> do
        (Array xs : r) <- readIORef stackRef
        writeIORef stackRef r
        case xs of
          [] -> word (Qualified Vocabulary.global "true") []
          _ : _ -> word (Qualified Vocabulary.global "false") []
      "head" -> do
        (Array xs : r) <- readIORef stackRef
        case xs of
          [] -> do
            writeIORef stackRef r
            word (Qualified Vocabulary.global "none") []
          x : _ -> do
            writeIORef stackRef $ x : r
            -- FIXME: Use right args.
            word (Qualified Vocabulary.global "some") []
      "prepend" -> do
        (Array xs : x : r) <- readIORef stackRef
        writeIORef stackRef $ Array (x : xs) : r
      "print" -> do
        (Array cs : r) <- readIORef stackRef
        writeIORef stackRef r
        mapM_ (\ (Character c) -> putChar c) cs
      "tail" -> do
        (Array xs : r) <- readIORef stackRef
        case xs of
          [] -> do
            writeIORef stackRef r
            word (Qualified Vocabulary.global "none") []
          _ : x -> do
            writeIORef stackRef $ Array x : r
            -- FIXME: Use right args.
            word (Qualified Vocabulary.global "some") []
      _ -> error "no such intrinsic"

      where

      binaryInt32 f = do
        (Integer y : Integer x : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral
            $ f (fromIntegral x :: Int32) (fromIntegral y :: Int32))
          : r

      binaryFloat64 f = do
        (Float y : Float x : r) <- readIORef stackRef
        writeIORef stackRef $ Float (f x y) : r

  word (fromMaybe mainName mName) mainArgs
  readIORef stackRef

data Failure = Failure Pretty.Doc
  deriving (Typeable)

instance Exception Failure

instance Show Failure where
  show (Failure message) = Pretty.render message
