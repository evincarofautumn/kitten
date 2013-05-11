{-# LANGUAGE RecordWildCards #-}

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
import Kitten.Def
import Kitten.Fragment
import Kitten.Interpret.Monad
import Kitten.Name
import Kitten.Resolved

import qualified Kitten.Builtin as Builtin

interpret
  :: [Value]
  -> [Def Resolved]
  -> Fragment Resolved
  -> IO ()
interpret stack prelude Fragment{..} = void $ evalStateT
  (mapM interpretTerm fragmentTerms) Env
  { envData = stack
  , envLocals = []
  , envDefs = prelude ++ fragmentDefs
  , envClosure = []
  , envLocations = []
  }              

interpretTerm :: Resolved -> Interpret
interpretTerm resolved = case resolved of
  Block terms -> mapM_ interpretTerm terms
  Builtin builtin _ -> interpretBuiltin builtin
  Closed name _ -> pushData =<< getClosed name
  If condition true false loc -> withLocation loc $ do
    mapM_ interpretTerm condition
    Bool test <- popData
    mapM_ interpretTerm $ if test then true else false
  Local name _ -> pushData =<< getLocal name
  Push value loc -> withLocation loc $ interpretValue value
  Scoped terms loc -> withLocation loc $ do
    pushLocal =<< popData
    mapM_ interpretTerm terms
    popLocal

interpretValue :: Value -> Interpret
interpretValue value = case value of
  Word (Name index) -> do
    Def _ term loc <- gets ((!! index) . envDefs)
    withLocation loc $ do
      interpretTerm term
      interpretBuiltin Builtin.Apply
  Closure _ names terms -> do
    values <- mapM getLocal names
    pushData $ Activation values terms
  _ -> pushData value

interpretFunction :: Value -> Interpret
interpretFunction function = case function of
  Activation values terms
    -> withClosure values $ mapM_ interpretTerm terms
  Escape (Name index) -> do
    Def _ term loc <- gets ((!! index) . envDefs)
    withLocation loc $ do
      interpretTerm term
      interpretBuiltin Builtin.Apply
  _ -> fail $ concat
    [ "attempt to apply non-function "
    , show function
    ]

interpretBuiltin :: Builtin -> Interpret
interpretBuiltin builtin = case builtin of
  Builtin.AddFloat -> floatsToFloat (+)
  Builtin.AddInt -> intsToInt (+)

  Builtin.AndBool -> boolsToBool (&&)

  Builtin.AndInt -> intsToInt (.&.)

  Builtin.Apply -> interpretFunction =<< popData

  Builtin.Bottom -> do
    Vector _ a <- popData
    pushData $ last a

  Builtin.CatVector -> do
    Vector _ b <- popData
    Vector _ a <- popData
    pushData $ Vector Nothing (a ++ b)

  Builtin.Compose -> do
    b <- popData
    a <- popData
    loc <- here
    pushData $ Activation []
      [ Push a loc
      , Builtin Builtin.Apply loc
      , Push b loc
      , Builtin Builtin.Apply loc
      ]

  Builtin.DecFloat -> floatToFloat pred
  Builtin.DecInt -> intToInt pred

  Builtin.DivFloat -> floatsToFloat (/)
  Builtin.DivInt -> intsToInt div

  Builtin.Down -> do
    Vector _ a <- popData
    pushData $ Vector Nothing (tail a)

  Builtin.Drop -> void popData

  Builtin.Dup -> do
    a <- popData
    pushData a
    pushData a

  Builtin.EqFloat -> floatsToBool (==)
  Builtin.EqInt -> intsToBool (==)
  Builtin.EqVector -> vectorsToBool (==)

  Builtin.Empty -> do
    Vector _ a <- popData
    pushData . Bool $ null a

  Builtin.Function -> fail
    "TODO interpretBuiltin Builtin.Function"

  Builtin.GeFloat -> floatsToBool (>=)
  Builtin.GeInt -> intsToBool (>=)
  Builtin.GeVector -> vectorsToBool (>=)

  Builtin.Get -> do
    Int b <- popData
    Vector _ a <- popData
    pushData $ a !! b

  Builtin.GetLine -> do
    line <- lift getLine
    pushData $ Vector Nothing (charsFromString line)

  Builtin.GtFloat -> floatsToBool (>)
  Builtin.GtInt -> intsToBool (>)
  Builtin.GtVector -> vectorsToBool (>)

  Builtin.IncFloat -> floatToFloat succ
  Builtin.IncInt -> intToInt succ

  Builtin.LeFloat -> floatsToBool (<=)
  Builtin.LeInt -> intsToBool (<=)
  Builtin.LeVector -> vectorsToBool (<=)

  Builtin.Length -> do
    Vector _ a <- popData
    pushData . Int $ length a

  Builtin.LtFloat -> floatsToBool (<)
  Builtin.LtInt -> intsToBool (<)
  Builtin.LtVector -> vectorsToBool (<)

  Builtin.ModFloat -> floatsToFloat mod'
  Builtin.ModInt -> intsToInt mod

  Builtin.MulFloat -> floatsToFloat (*)
  Builtin.MulInt -> intsToInt (*)

  Builtin.NeFloat -> floatsToBool (/=)
  Builtin.NeInt -> intsToBool (/=)
  Builtin.NeVector -> vectorsToBool (/=)

  Builtin.NegFloat -> floatToFloat negate
  Builtin.NegInt -> intToInt negate

  Builtin.NotBool -> boolToBool not

  Builtin.NotInt -> intToInt complement

  Builtin.OrBool -> boolsToBool (||)

  Builtin.OrInt -> intsToInt (.|.)

  Builtin.Print -> do
    Vector _ a <- popData
    lift $ putStr (stringFromChars a) >> hFlush stdout

  Builtin.Set -> do
    Int c <- popData
    b <- popData
    Vector _ a <- popData
    pushData . Vector Nothing
      $ let (before, after) = splitAt c a
      in before ++ b : drop 1 after

  Builtin.ShowFloat -> do
    Float value <- popData
    pushData $ Vector Nothing (charsFromString $ show value)

  Builtin.ShowInt -> do
    Int value <- popData
    pushData $ Vector Nothing (charsFromString $ show value)

  Builtin.SubFloat -> floatsToFloat (-)
  Builtin.SubInt -> intsToInt (-)

  Builtin.Swap -> do
    b <- popData
    a <- popData
    pushData b
    pushData a

  Builtin.Top -> do
    Vector _ a <- popData
    pushData $ head a

  Builtin.Up -> do
    Vector _ a <- popData
    pushData $ Vector Nothing (init a)

  Builtin.Vector -> do
    a <- popData
    pushData $ Vector Nothing [a]

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

  vectorsToBool :: (String -> String -> Bool) -> Interpret
  vectorsToBool f = do
    Vector _ b <- popData
    Vector _ a <- popData
    pushData . Bool $ f (stringFromChars a) (stringFromChars b)

stringFromChars :: [Value] -> String
stringFromChars = map $ \ (Char c) -> c

charsFromString :: String -> [Value]
charsFromString = map Char
