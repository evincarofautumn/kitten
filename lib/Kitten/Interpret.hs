{-# LANGUAGE RecordWildCards #-}

module Kitten.Interpret
  ( interpret
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bits
import Data.Fixed
import System.Exit
import System.IO

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Interpret.Monad
import Kitten.Name
import Kitten.Resolved
import Kitten.Util.Void

import qualified Kitten.Builtin as Builtin

interpret
  :: [Value]
  -> Fragment Value Void
  -> Fragment Value Resolved
  -> IO ()
interpret stack prelude fragment = void $ evalStateT
  (mapM interpretTerm (fragmentTerms fragment)) Env
  { envData = stack
  , envLocals = []
  , envDefs = fragmentDefs prelude ++ fragmentDefs fragment
  , envClosure = []
  , envLocations = []
  }              

interpretTerm :: Resolved -> Interpret
interpretTerm resolved = case resolved of
  Builtin builtin _ -> interpretBuiltin builtin

  Call name _ -> interpretOverload name

  Compose terms -> mapM_ interpretTerm terms

  If true false loc -> withLocation loc $ do
    Bool test <- popData
    interpretTerm $ if test then true else false

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

  VectorTerm terms loc -> withLocation loc $ do
    mapM_ interpretTerm terms
    values <- replicateM (length terms) popData
    pushData $ Vector (reverse values)

interpretValue :: Value -> Interpret
interpretValue value = case value of
  Closed name -> pushData =<< getClosed name
  Closure names term -> do
    values <- mapM getClosedName names
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
  _ -> fail "attempt to apply non-function"

interpretOverload :: Name -> Interpret
interpretOverload (Name index) = do
  Def{..} <- gets ((!! index) . envDefs)
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
    pushData $ Vector (a ++ b)

  Builtin.AndBool -> boolsToBool (&&)

  Builtin.AndInt -> intsToInt (.&.)

  Builtin.Apply01 -> apply
  Builtin.Apply11 -> apply
  Builtin.Apply21 -> apply

  Builtin.Call01 -> apply
  Builtin.Call10 -> apply

  Builtin.CharToInt -> do
    Char a <- popData
    pushData $ Int (fromEnum a)

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

  Builtin.GeFloat -> floatsToBool (>=)
  Builtin.GeInt -> intsToBool (>=)

  Builtin.Get -> do
    Int b <- popData
    Vector a <- popData
    pushData $ a !! b

  Builtin.GetLine -> do
    Handle a <- popData
    line <- lift $ hGetLine a
    pushData $ Vector (charsFromString line)

  Builtin.GtFloat -> floatsToBool (>)
  Builtin.GtInt -> intsToBool (>)

  Builtin.Impure -> return ()

  Builtin.Init -> do
    Vector a <- popData
    pushData $ Vector (init a)

  Builtin.LeFloat -> floatsToBool (<=)
  Builtin.LeInt -> intsToBool (<=)

  Builtin.Length -> do
    Vector a <- popData
    pushData . Int $ length a

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

  Builtin.NotBool -> boolToBool not

  Builtin.NotInt -> intToInt complement

  Builtin.OrBool -> boolsToBool (||)

  Builtin.OrInt -> intsToInt (.|.)

  Builtin.OpenIn -> do
    Vector a <- popData
    let fileName = stringFromChars a
    handle <- lift $ openFile fileName ReadMode
    pushData $ Handle handle

  Builtin.OpenOut -> do
    Vector a <- popData
    let fileName = stringFromChars a
    handle <- lift $ openFile fileName WriteMode
    pushData $ Handle handle

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

  Builtin.Set -> do
    Int c <- popData
    b <- popData
    Vector a <- popData
    pushData . Vector
      $ let (before, after) = splitAt c a
      in before ++ b : drop 1 after

  Builtin.ShowFloat -> do
    Float value <- popData
    pushData $ Vector (charsFromString $ show value)

  Builtin.ShowInt -> do
    Int value <- popData
    pushData $ Vector (charsFromString $ show value)

  Builtin.Stderr -> pushData $ Handle stderr
  Builtin.Stdin -> pushData $ Handle stdin
  Builtin.Stdout -> pushData $ Handle stdout

  Builtin.SubFloat -> floatsToFloat (-)
  Builtin.SubInt -> intsToInt (-)

  Builtin.Tail -> do
    Vector a <- popData
    pushData $ Vector (tail a)

  Builtin.UnsafePurify11 -> return ()

  Builtin.Vector -> do
    a <- popData
    pushData $ Vector [a]

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

stringFromChars :: [Value] -> String
stringFromChars = map fromChar
  where
  fromChar :: Value -> Char
  fromChar (Char c) = c
  fromChar _ = error "Kitten.Interpret.stringFromChars: non-character"

charsFromString :: String -> [Value]
charsFromString = map Char
