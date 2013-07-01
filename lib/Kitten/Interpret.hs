module Kitten.Interpret
  ( interpret
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bits
import Data.Fixed
import System.IO

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Def
import Kitten.Fragment
import Kitten.Interpret.Monad
import Kitten.Name
import Kitten.Typed
import Kitten.Util.Void

import qualified Kitten.Builtin as Builtin

interpret
  :: [Value]
  -> Fragment Value Void
  -> Fragment Value Typed
  -> IO ()
interpret stack prelude fragment = void $ evalStateT
  (mapM interpretTerm (fragmentTerms fragment)) Env
  { envData = stack
  , envLocals = []
  , envDefs = fragmentDefs prelude ++ fragmentDefs fragment
  , envClosure = []
  , envLocations = []
  }              

interpretTerm :: Typed -> Interpret
interpretTerm resolved = case resolved of
  Call name _ -> interpretOverload name
  Compose terms -> mapM_ interpretTerm terms
  Builtin builtin _ -> interpretBuiltin builtin
  If true false loc -> withLocation loc $ do
    Bool test <- popData
    interpretTerm $ if test then true else false
  Push value loc -> withLocation loc $ interpretValue value
  Scoped term loc -> withLocation loc $ do
    pushLocal =<< popData
    interpretTerm term
    popLocal

interpretValue :: Value -> Interpret
interpretValue value = case value of
  Closed name -> pushData =<< getClosed name
  Closure names term -> do
    values <- mapM getClosedName names
    pushData $ Activation values term
    where
    getClosedName :: ClosedName -> InterpretM Value
    getClosedName (ClosedName name) = getLocal name
    getClosedName (ReclosedName name) = getClosed name
  Local name -> pushData =<< getLocal name
  _ -> pushData value

interpretFunction :: Value -> Interpret
interpretFunction function = case function of
  Activation values term
    -> withClosure values $ interpretTerm term
  Escape names -> interpretOverload names
  _ -> fail $ "attempt to apply non-function " ++ show function

interpretOverload :: Name -> Interpret
interpretOverload (Name index) = do
  Def _ term loc <- gets ((!! index) . envDefs)
  withLocation loc $ do
    interpretValue term
    interpretFunction =<< popData

interpretBuiltin :: Builtin -> Interpret
interpretBuiltin builtin = case builtin of
  Builtin.AddFloat -> floatsToFloat (+)
  Builtin.AddInt -> intsToInt (+)

  Builtin.AddVector -> do
    Vector b <- popData
    Vector a <- popData
    pushData $ Vector (b ++ a)

  Builtin.AndBool -> boolsToBool (&&)

  Builtin.AndInt -> intsToInt (.&.)

  Builtin.Apply10 -> interpretFunction =<< popData
  Builtin.Apply11 -> interpretFunction =<< popData
  Builtin.Apply21 -> interpretFunction =<< popData

  Builtin.Bottom -> do
    Vector a <- popData
    pushData $ last a

  Builtin.Close -> do
    Handle a <- popData
    lift $ hClose a

  Builtin.DecFloat -> floatToFloat pred
  Builtin.DecInt -> intToInt pred

  Builtin.DivFloat -> floatsToFloat (/)
  Builtin.DivInt -> intsToInt div

  Builtin.Down -> do
    Vector a <- popData
    pushData $ Vector (tail a)

  Builtin.Drop -> void popData

  Builtin.Dup -> do
    a <- popData
    pushData a
    pushData a

  Builtin.EqChar -> charsToBool (==)
  Builtin.EqFloat -> floatsToBool (==)
  Builtin.EqInt -> intsToBool (==)
  Builtin.EqVector -> vectorsToBool (==)

  Builtin.Empty -> do
    Vector a <- popData
    pushData . Bool $ null a

  Builtin.First -> do
    Pair a _ <- popData
    pushData a

  Builtin.Function -> fail
    "TODO interpretBuiltin Builtin.Function"

  Builtin.GeChar -> charsToBool (>=)
  Builtin.GeFloat -> floatsToBool (>=)
  Builtin.GeInt -> intsToBool (>=)
  Builtin.GeVector -> vectorsToBool (>=)

  Builtin.Get -> do
    Int b <- popData
    Vector a <- popData
    pushData $ a !! b

  Builtin.GetLine -> do
    Handle a <- popData
    line <- lift $ hGetLine a
    pushData $ Vector (charsFromString line)

  Builtin.GtChar -> charsToBool (>)
  Builtin.GtFloat -> floatsToBool (>)
  Builtin.GtInt -> intsToBool (>)
  Builtin.GtVector -> vectorsToBool (>)

  Builtin.IncFloat -> floatToFloat succ
  Builtin.IncInt -> intToInt succ

  Builtin.LeChar -> charsToBool (<=)
  Builtin.LeFloat -> floatsToBool (<=)
  Builtin.LeInt -> intsToBool (<=)
  Builtin.LeVector -> vectorsToBool (<=)

  Builtin.Length -> do
    Vector a <- popData
    pushData . Int $ length a

  Builtin.LtChar -> charsToBool (<)
  Builtin.LtFloat -> floatsToBool (<)
  Builtin.LtInt -> intsToBool (<)
  Builtin.LtVector -> vectorsToBool (<)

  Builtin.ModFloat -> floatsToFloat mod'
  Builtin.ModInt -> intsToInt mod

  Builtin.MulFloat -> floatsToFloat (*)
  Builtin.MulInt -> intsToInt (*)

  Builtin.NeChar -> charsToBool (/=)
  Builtin.NeFloat -> floatsToBool (/=)
  Builtin.NeInt -> intsToBool (/=)
  Builtin.NeVector -> vectorsToBool (/=)

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

  Builtin.Swap -> do
    b <- popData
    a <- popData
    pushData b
    pushData a

  Builtin.Top -> do
    Vector a <- popData
    pushData $ head a

  Builtin.Up -> do
    Vector a <- popData
    pushData $ Vector (init a)

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

  charsToBool :: (Char -> Char -> Bool) -> Interpret
  charsToBool f = do
    mb <- popData
    b <- case mb of
      Char b -> return b
      _ -> error $ "expected Char but got " ++ show mb
    ma <- popData
    a <- case ma of
      Char a -> return a
      _ -> error $ "expected Char but got " ++ show ma
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

  vectorsToBool :: (String -> String -> Bool) -> Interpret
  vectorsToBool f = do
    Vector b <- popData
    Vector a <- popData
    pushData . Bool $ f (stringFromChars a) (stringFromChars b)

stringFromChars :: [Value] -> String
stringFromChars = map fromChar . reverse
  where
  fromChar :: Value -> Char
  fromChar (Char c) = c
  fromChar value = error
    $ "Kitten.Interpret.stringFromChars: "
    ++ show value

charsFromString :: String -> [Value]
charsFromString = map Char . reverse
