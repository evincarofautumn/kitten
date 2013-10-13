{-# LANGUAGE RecordWildCards #-}

module Kitten.Interpret
  ( interpret
  ) where

import Control.Applicative hiding (some)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bits
import Data.Fixed
import Data.Monoid
import Data.Vector ((!))
import System.Exit
import System.IO

import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Interpret.Monad
import Kitten.Name
import Kitten.Resolved

import qualified Kitten.Builtin as Builtin

interpret
  :: [Value]
  -> Fragment Resolved
  -> Fragment Resolved
  -> IO [Value]
interpret stack prelude fragment = liftM envData $ execStateT
  (F.mapM_ interpretTerm (fragmentTerms fragment)) Env
  { envData = stack
  , envLocals = []
  , envDefs = fragmentDefs prelude <> fragmentDefs fragment
  , envClosure = V.empty
  , envLocations = []
  }              

interpretTerm :: Resolved -> Interpret
interpretTerm resolved = case resolved of
  Builtin builtin loc -> withLocation loc $ interpretBuiltin builtin
  Call name loc -> withLocation loc $ interpretOverload name
  Compose terms loc -> withLocation loc
    $ F.mapM_ interpretTerm terms
  From _ loc -> withLocation loc $ do
    Wrapped _ value <- popData
    pushData value
  PairTerm a b loc -> withLocation loc $ do
    interpretTerm a
    a' <- popData
    interpretTerm b
    b' <- popData
    pushData $ Pair a' b'
  Push value loc -> withLocation loc $ interpretValue value
  Scoped term loc -> withLocation loc $ do
    pushLocal =<< popData
    interpretTerm term
    popLocal
  To name loc -> withLocation loc $ do
    a <- popData
    pushData $ Wrapped name a
  VectorTerm terms loc -> withLocation loc $ do
    F.mapM_ interpretTerm terms
    values <- V.fromList <$> replicateM (V.length terms) popData
    pushData $ Vector (V.reverse values)

interpretValue :: Value -> Interpret
interpretValue value = case value of
  Closed name -> pushData =<< getClosed name
  Closure names term -> do
    values <- T.mapM getClosedName names
    pushData $ Activation values term
  Local name -> pushData =<< getLocal name
  _ -> pushData value

getClosedName :: ClosedName -> InterpretM Value
getClosedName (ClosedName name) = getLocal name
getClosedName (ReclosedName name) = getClosed name

interpretFunction :: Value -> Interpret
interpretFunction function = case function of
  Activation values term
    -> withClosure values $ interpretTerm term
  _ -> do
    loc <- here
    fail $ show loc ++ ": attempt to apply non-function"

interpretOverload :: Name -> Interpret
interpretOverload (Name index) = do
  Def{..} <- gets ((! index) . envDefs)
  withLocation defLocation $ do
    interpretValue defTerm
    apply

apply :: Interpret
apply = interpretFunction =<< popData

interpretBuiltin :: Builtin -> Interpret
interpretBuiltin builtin = case builtin of
  Builtin.AddFloat -> floatsToFloat (+)
  Builtin.AddInt -> intsToInt (+)

  Builtin.AddVector -> do
    Vector b <- popData
    Vector a <- popData
    pushData $ Vector (a <> b)

  Builtin.AndBool -> boolsToBool (&&)

  Builtin.AndInt -> intsToInt (.&.)

  Builtin.Apply -> apply

  Builtin.CharToInt -> do
    Char a <- popData
    pushData $ Int (fromEnum a)

  Builtin.Choice -> do
    left <- popData
    Choice which value <- popData
    unless which $ pushData value >> interpretFunction left

  Builtin.ChoiceElse -> do
    right <- popData
    left <- popData
    Choice which value <- popData
    pushData value
    interpretFunction $ if which then right else left

  Builtin.Close -> do
    Handle a <- popData
    lift $ hClose a

  Builtin.DivFloat -> floatsToFloat (/)
  Builtin.DivInt -> intsToInt div

  Builtin.EqFloat -> floatsToBool (==)
  Builtin.EqInt -> intsToBool (==)

  Builtin.Exit -> do
    Int a <- popData
    lift $ case a of
      0 -> exitSuccess
      _ -> exitWith (ExitFailure a)

  Builtin.First -> do
    Pair a _ <- popData
    pushData a

  Builtin.FromLeft -> do
    Choice False a <- popData
    pushData a

  Builtin.FromRight -> do
    Choice True a <- popData
    pushData a

  Builtin.FromSome -> do
    Option (Just a) <- popData
    pushData a

  Builtin.GeFloat -> floatsToBool (>=)
  Builtin.GeInt -> intsToBool (>=)

  Builtin.Get -> do
    Int b <- popData
    Vector a <- popData
    pushData . Option $ if b >= 0 && b < V.length a
      then Just (a ! b)
      else Nothing

  Builtin.GetLine -> do
    Handle a <- popData
    line <- lift $ hGetLine a
    pushData $ Vector (charsFromString line)

  Builtin.GtFloat -> floatsToBool (>)
  Builtin.GtInt -> intsToBool (>)

  Builtin.If -> do
    true <- popData
    Bool test <- popData
    when test $ interpretFunction true

  Builtin.IfElse -> do
    false <- popData
    true <- popData
    Bool test <- popData
    interpretFunction $ if test then true else false

  Builtin.Impure -> return ()

  Builtin.Init -> do
    Vector a <- popData
    pushData . Vector $ if V.null a
      then V.empty
      else V.init a

  Builtin.IntToChar -> do
    Int a <- popData
    pushData . Option $ if a >= 0 && a <= 0x10FFFF
      then Just $ Char (toEnum a)
      else Nothing

  Builtin.LeFloat -> floatsToBool (<=)
  Builtin.LeInt -> intsToBool (<=)

  Builtin.Left -> do
    a <- popData
    pushData $ Choice False a

  Builtin.Length -> do
    Vector a <- popData
    pushData . Int $ V.length a

  Builtin.LtFloat -> floatsToBool (<)
  Builtin.LtInt -> intsToBool (<)

  Builtin.ModFloat -> floatsToFloat mod'
  Builtin.ModInt -> intsToInt mod

  Builtin.MulFloat -> floatsToFloat (*)
  Builtin.MulInt -> intsToInt (*)

  Builtin.NeFloat -> floatsToBool (/=)
  Builtin.NeInt -> intsToBool (/=)

  Builtin.NegFloat -> floatToFloat negate
  Builtin.NegInt -> intToInt negate

  Builtin.None -> pushData $ Option Nothing

  Builtin.NotBool -> boolToBool not
  Builtin.NotInt -> intToInt complement

  Builtin.OrBool -> boolsToBool (||)
  Builtin.OrInt -> intsToInt (.|.)

  Builtin.OpenIn -> openFilePushHandle ReadMode
  Builtin.OpenOut -> openFilePushHandle WriteMode

  Builtin.Option -> do
    some <- popData
    Option mValue <- popData
    case mValue of
      Just value -> pushData value >> interpretFunction some
      Nothing -> return ()

  Builtin.OptionElse -> do
    none <- popData
    some <- popData
    Option mValue <- popData
    case mValue of
      Just value -> pushData value >> interpretFunction some
      Nothing -> interpretFunction none

  Builtin.Pair -> do
    b <- popData
    a <- popData
    pushData $ Pair a b

  Builtin.Print -> do
    Handle b <- popData
    Vector a <- popData
    lift $ hPutStr b (stringFromChars a) >> hFlush b

  Builtin.Rest -> do
    Pair _ b <- popData
    pushData b

  Builtin.Right -> do
    a <- popData
    pushData $ Choice True a

  Builtin.Set -> do
    Int c <- popData
    b <- popData
    Vector a <- popData
    pushData . Vector
      $ let (before, after) = V.splitAt c a
      in before <> V.singleton b <> V.drop 1 after

  Builtin.ShowFloat -> do
    Float value <- popData
    pushData $ Vector (charsFromString $ show value)

  Builtin.ShowInt -> do
    Int value <- popData
    pushData $ Vector (charsFromString $ show value)

  Builtin.Some -> do
    a <- popData
    pushData $ Option (Just a)

  Builtin.Stderr -> pushData $ Handle stderr
  Builtin.Stdin -> pushData $ Handle stdin
  Builtin.Stdout -> pushData $ Handle stdout

  Builtin.SubFloat -> floatsToFloat (-)
  Builtin.SubInt -> intsToInt (-)

  Builtin.Tail -> do
    Vector a <- popData
    pushData . Vector $ if V.null a
      then V.empty
      else V.tail a

  Builtin.UnsafePurify11 -> return ()

  Builtin.XorBool -> boolsToBool (/=)

  Builtin.XorInt -> intsToInt xor

  where

  boolToBool :: (Bool -> Bool) -> Interpret
  boolToBool f = do
    Bool a <- popData
    pushData $ Bool (f a)

  boolsToBool :: (Bool -> Bool -> Bool) -> Interpret
  boolsToBool f = do
    Bool b <- popData
    Bool a <- popData
    pushData $ Bool (f a b)

  floatToFloat :: (Double -> Double) -> Interpret
  floatToFloat f = do
    Float a <- popData
    pushData $ Float (f a)

  floatsToBool :: (Double -> Double -> Bool) -> Interpret
  floatsToBool f = do
    Float b <- popData
    Float a <- popData
    pushData $ Bool (f a b)

  floatsToFloat :: (Double -> Double -> Double) -> Interpret
  floatsToFloat f = do
    Float b <- popData
    Float a <- popData
    pushData $ Float (f a b)

  intToInt :: (Int -> Int) -> Interpret
  intToInt f = do
    Int a <- popData
    pushData $ Int (f a)

  intsToBool :: (Int -> Int -> Bool) -> Interpret
  intsToBool f = do
    Int b <- popData
    Int a <- popData
    pushData $ Bool (f a b)

  intsToInt :: (Int -> Int -> Int) -> Interpret
  intsToInt f = do
    Int b <- popData
    Int a <- popData
    pushData $ Int (f a b)

  openFilePushHandle :: IOMode -> Interpret
  openFilePushHandle ioMode = do
    Vector a <- popData
    let fileName = stringFromChars a
    handle <- lift $ openFile fileName ioMode
    pushData $ Handle handle
