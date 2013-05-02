{-# LANGUAGE RecordWildCards #-}

module Kitten.Interpret
  ( interpret
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Bits

import Kitten.Builtin (Builtin)
import Kitten.Def
import Kitten.Fragment
import Kitten.Location
import Kitten.Name
import Kitten.Resolve (Resolved(..), Value(..))

import qualified Kitten.Builtin as Builtin

data Env = Env
  { envData :: [Value]
  , envLocals :: [Value]
  , envDefs :: [Def Resolved]
  , envClosure :: [Value]
  , envLocations :: [Location]
  }

type InterpretM a = StateT Env IO a
type Interpret = InterpretM ()

interpret :: [Value] -> Fragment Resolved -> IO ()
interpret stack Fragment{..} = void $ evalStateT
  (mapM interpretTerm fragmentTerms) Env
  { envData = stack
  , envLocals = []
  , envDefs = fragmentDefs
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
  Closure names terms -> do
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
  Builtin.Add -> intsToInt (+)

  Builtin.AndBool -> boolsToBool (&&)

  Builtin.AndInt -> intsToInt (.&.)

  Builtin.Apply -> interpretFunction =<< popData

  Builtin.At -> do
    Int b <- popData
    Vector _ a <- popData
    pushData $ a !! b

  Builtin.Bottom -> do
    Vector _ a <- popData
    pushData $ last a

  Builtin.Cat -> do
    Vector _ b <- popData
    Vector _ a <- popData
    pushData $ Vector Nothing (b ++ a)

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

  Builtin.Div -> intsToInt div

  Builtin.Down -> do
    Vector _ a <- popData
    pushData $ Vector Nothing (tail a)

  Builtin.Drop -> void popData

  Builtin.Dup -> do
    a <- popData
    pushData a
    pushData a

  Builtin.Eq -> intsToBool (==)

  Builtin.Empty -> do
    Vector _ a <- popData
    pushData . Bool $ null a

  Builtin.Function -> fail
    "TODO interpretBuiltin Builtin.Function"

  Builtin.Ge -> intsToBool (>=)

  Builtin.Gt -> intsToBool (>)

  Builtin.Le -> intsToBool (<=)

  Builtin.Length -> do
    Vector _ a <- popData
    pushData . Int $ length a

  Builtin.Lt -> intsToBool (<)

  Builtin.Mod -> intsToInt mod

  Builtin.Mul -> intsToInt (*)

  Builtin.Ne -> intsToBool (/=)

  Builtin.Neg -> intToInt negate

  Builtin.NotBool -> boolToBool not

  Builtin.NotInt -> intToInt complement

  Builtin.OrBool -> boolsToBool (||)

  Builtin.OrInt -> intsToInt (.|.)

  Builtin.Print -> do
    Text text <- popData
    lift $ putStr text

  Builtin.ShowInt -> do
    Int value <- popData
    pushData $ Text (show value)

  Builtin.Sub -> intsToInt (-)

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

  intsToBool :: (Int -> Int -> Bool) -> Interpret
  intsToBool f = do
    Int b <- popData
    Int a <- popData
    pushData $ Bool (f a b)

  intToInt :: (Int -> Int) -> Interpret
  intToInt f = do
    Int a <- popData
    pushData $ Int (f a)

  intsToInt :: (Int -> Int -> Int) -> Interpret
  intsToInt f = do
    Int b <- popData
    Int a <- popData
    pushData $ Int (f a b)

pushData :: Value -> Interpret
pushData value = modify $ \ env@Env{..}
  -> env { envData = value : envData }

popData :: InterpretM Value
popData = do
  dataStack <- gets envData
  case dataStack of
    [] -> fail "stack underflow"
    (top : down) -> do
      modify $ \ env -> env { envData = down }
      return top

getLocal :: Name -> InterpretM Value
getLocal (Name index) = do
  locals <- gets envLocals
  return $ locals !! index

getClosed :: Name -> InterpretM Value
getClosed (Name index) = do
  closure <- gets envClosure
  return $ closure !! index

pushLocal :: Value -> Interpret
pushLocal value = modify $ \ env@Env{..}
  -> env { envLocals = value : envLocals }

popLocal :: Interpret
popLocal = do
  localStack <- gets envLocals
  case localStack of
    [] -> fail "local stack underflow"
    (_ : down) -> do
      modify $ \ env -> env { envLocals = down }

withClosure :: [Value] -> InterpretM a -> InterpretM a
withClosure values action = do
  closure <- gets envClosure
  modify $ \ env@Env{..} -> env { envClosure = values }
  result <- action
  modify $ \ env@Env{..} -> env { envClosure = closure }
  return result

here :: InterpretM Location
here = do
  locations <- gets envLocations
  return $ case locations of
    [] -> UnknownLocation
    (location : _) -> location

withLocation :: Location -> InterpretM a -> InterpretM a
withLocation location action = do
  modify $ \ env@Env{..} -> env
    { envLocations = location : envLocations }
  result <- action
  modify $ \ env@Env{..} -> env
    { envLocations = tail envLocations }
  return result
