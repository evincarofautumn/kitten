{-|
Module      : Kitten.Interpret
Description : Simple interpreter
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Interpret
  ( Failure
  , interpret
  ) where

import Control.Exception (Exception, throwIO)
import Data.Fixed (mod')
import Data.Foldable (toList)
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import Data.Int
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Data.Vector ((!))
import Data.Word
import Kitten.Bits
import Kitten.Definition (mainName)
import Kitten.Dictionary (Dictionary)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Monad (runKitten)
import Kitten.Name
import Kitten.Stack (Stack((:::)))
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Type(..))
import System.Exit (ExitCode(..), exitWith)
import System.IO (Handle, hGetLine, hFlush, hPutChar, hPutStrLn)
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError,
                        isPermissionError)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import Text.Printf (hPrintf)
import qualified Codec.Picture.Png as Png
import qualified Codec.Picture.Types as Picture
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Report as Report
import qualified Kitten.Stack as Stack
import qualified Kitten.Term as Term
import qualified Kitten.TypeEnv as TypeEnv
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty
import Control.Exception (ArithException(..), catch)

-- | Interprets a program dictionary.

interpret
  :: Dictionary
  -- ^ The program.
  -> Maybe Qualified
  -- ^ The name of the entry point, if overriding @main@.
  -> [Type]
  -- ^ Arguments passed to @main@.
  -> Handle
  -- ^ Standard input handle.
  -> Handle
  -- ^ Standard output handle.
  -> Handle
  -- ^ Standard error handle.
  -> [Value Type]
  -- ^ Initial stack state.
  -> IO [Value Type]
  -- ^ Final stack state.
interpret dictionary mName mainArgs stdin' stdout' _stderr' initialStack = do
  -- TODO: Types.
  stackRef <- newIORef $ Stack.fromList initialStack
  localsRef <- newIORef []
  currentClosureRef <- newIORef []
  let

    word :: [Qualified] -> Qualified -> [Type] -> IO ()
    word callStack name args = do
      let mangled = Instantiated name args
      case Dictionary.lookup mangled dictionary of
        -- An entry in the dictionary should already be instantiated, so we
        -- shouldn't need to instantiate it again here.
        Just (Entry.Word _ _ _ _ _ (Just body)) -> term (name : callStack) body
        _ -> case Dictionary.lookup (Instantiated name []) dictionary of
          -- A regular word.
          Just (Entry.Word _ _ _ _ _ (Just body)) -> do
            mBody' <- runKitten $ Instantiate.term TypeEnv.empty body args
            case mBody' of
              Right body' -> term (name : callStack) body'
              Left reports -> hPutStrLn stdout' $ Pretty.render $ Pretty.vcat
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
              -> intrinsic (name : callStack) unqualified
            _ -> error "no such intrinsic"
          _ -> throwIO $ Failure $ Pretty.hcat
            [ "I can't find an instantiation of "
            , Pretty.quote name
            , ": "
            , Pretty.quote mangled
            ]

    term :: [Qualified] -> Term Type -> IO ()
    term callStack t = case t of
      Coercion{} -> return ()
      Compose _ a b -> term callStack a >> term callStack b
      -- TODO: Verify that this is correct.
      Generic _name _ t' _ -> term callStack t'
      Group t' -> term callStack t'
      Lambda _ _name _ body _ -> do
        a ::: r <- readIORef stackRef
        ls <- readIORef localsRef
        writeIORef stackRef r
        writeIORef localsRef (a : ls)
        term callStack body
        modifyIORef' localsRef tail
      Match _ _ cases else_ _ -> do
        -- We delay matching on the value here because it may not be an ADT at
        -- all. For example, "1 match else { 2 }" is perfectly valid,
        -- because we are matching on all (0) of Int32's constructors.
        x ::: r <- readIORef stackRef
        writeIORef stackRef r
        let
          go (Case (QualifiedName name) caseBody _ : _)
            -- FIXME: Embed this information during name resolution, rather than
            -- looking it up.
            | Just (Entry.Word _ _ _ _ _ (Just ctorBody))
              <- Dictionary.lookup (Instantiated name []) dictionary
            , [New _ (ConstructorIndex index') _ _] <- Term.decompose ctorBody
            , Algebraic (ConstructorIndex index) fields <- x
            , index == index'
            = do
              writeIORef stackRef $ Stack.pushes fields r
              term callStack caseBody
          go (_ : rest) = go rest
          go [] = case else_ of
            Else body _ -> term callStack body
        go cases
      New _ index size _ -> do
        r <- readIORef stackRef
        let (fields, r') = Stack.pops size r
        writeIORef stackRef $ Algebraic index fields ::: r'
      NewClosure _ size _ -> do
        r <- readIORef stackRef
        let (Name name : closure, r') = Stack.pops (size + 1) r
        writeIORef stackRef (Closure name (reverse closure) ::: r')
      NewVector _ size _ _ -> do
        r <- readIORef stackRef
        let (values, r') = Stack.pops size r
        writeIORef stackRef
          $ Array (Vector.reverse $ Vector.fromList values) ::: r'
      Push _ value _ -> push value
      Word _ _ (QualifiedName name) args _
        -> word callStack name args
      -- FIXME: Use proper reporting. (Internal error?)
      Word _ _ name _ _ -> error $ Pretty.render $ Pretty.hsep
        ["unresolved word name", pPrint name]

    call :: [Qualified] -> IO ()
    call callStack = do
      Closure name closure ::: r <- readIORef stackRef
      writeIORef stackRef r
      modifyIORef' currentClosureRef (closure :)
      -- FIXME: Use right args.
      word (name : callStack) name []
      modifyIORef' currentClosureRef tail

    push :: Value Type -> IO ()
    push value = case value of
      Closed (ClosureIndex index) -> do
        (currentClosure : _) <- readIORef currentClosureRef
        modifyIORef' stackRef ((currentClosure !! index) :::)
      Local (LocalIndex index) -> do
        locals <- readIORef localsRef
        modifyIORef' stackRef ((locals !! index) :::)
      Text text -> modifyIORef' stackRef
        ((Array $ fmap Character $ Vector.fromList $ Text.unpack text) :::)
      _ -> modifyIORef' stackRef (value :::)

    intrinsic :: [Qualified] -> Unqualified -> IO ()
    intrinsic callStack name = case name of
      "abort" -> do
        Array cs ::: r <- readIORef stackRef
        writeIORef stackRef r
        let message = map (\ (Character c) -> c) $ Vector.toList cs
        throwIO $ Failure $ Pretty.vcat
          $ Pretty.hsep
            [ "Execution failure:"
            , Pretty.text message
            ]
          : "Call stack:"
          : map (Pretty.nest 4 . pPrint) callStack

      "exit" -> do
        Integer i _ ::: r <- readIORef stackRef
        writeIORef stackRef r
        exitWith $ if i == 0 then ExitSuccess else ExitFailure (fromInteger i)

      "call" -> call callStack

      "drop" -> modifyIORef' stackRef Stack.pop'

      "swap" -> do
        a ::: b ::: r <- readIORef stackRef
        writeIORef stackRef $ b ::: a ::: r

      "neg_int8" -> unaryInt8 negate
      "add_int8" -> binaryInt8 (+)
      "sub_int8" -> binaryInt8 (-)
      "mul_int8" -> binaryInt8 (*)
      "div_int8" -> catchDivideByZero $ binaryInt8 div
      "mod_int8" -> catchDivideByZero $ binaryInt8 mod

      "neg_int16" -> unaryInt16 negate
      "add_int16" -> binaryInt16 (+)
      "sub_int16" -> binaryInt16 (-)
      "mul_int16" -> binaryInt16 (*)
      "div_int16" -> catchDivideByZero $ binaryInt16 div
      "mod_int16" -> catchDivideByZero $ binaryInt16 mod

      "neg_int32" -> unaryInt32 negate
      "add_int32" -> binaryInt32 (+)
      "sub_int32" -> binaryInt32 (-)
      "mul_int32" -> binaryInt32 (*)
      "div_int32" -> catchDivideByZero $ binaryInt32 div
      "mod_int32" -> catchDivideByZero $ binaryInt32 mod

      "neg_int64" -> unaryInt64 negate
      "add_int64" -> binaryInt64 (+)
      "sub_int64" -> binaryInt64 (-)
      "mul_int64" -> binaryInt64 (*)
      "div_int64" -> catchDivideByZero $ binaryInt64 div
      "mod_int64" -> catchDivideByZero $ binaryInt64 mod

      "not_int8" -> unaryInt8 complement
      "or_int8"  -> binaryInt8 (.|.)
      "and_int8" -> binaryInt8 (.&.)
      "xor_int8" -> binaryInt8 xor
      "shl_int8" -> binaryInt8Int shift
      "rol_int8" -> binaryInt8Int rotate

      "not_int16" -> unaryInt16 complement
      "or_int16"  -> binaryInt16 (.|.)
      "and_int16" -> binaryInt16 (.&.)
      "xor_int16" -> binaryInt16 xor
      "shl_int16" -> binaryInt16Int shift
      "rol_int16" -> binaryInt16Int rotate

      "not_int32" -> unaryInt32 complement
      "or_int32"  -> binaryInt32 (.|.)
      "and_int32" -> binaryInt32 (.&.)
      "xor_int32" -> binaryInt32 xor
      "shl_int32" -> binaryInt32Int shift
      "rol_int32" -> binaryInt32Int rotate

      "not_int64" -> unaryInt64 complement
      "or_int64"  -> binaryInt64 (.|.)
      "and_int64" -> binaryInt64 (.&.)
      "xor_int64" -> binaryInt64 xor
      "shl_int64" -> binaryInt64Int shift
      "rol_int64" -> binaryInt64Int rotate

      "lt_int8" -> boolInt8 (<)
      "gt_int8" -> boolInt8 (>)
      "le_int8" -> boolInt8 (<=)
      "ge_int8" -> boolInt8 (>=)
      "eq_int8" -> boolInt8 (==)
      "ne_int8" -> boolInt8 (/=)

      "lt_int16" -> boolInt16 (<)
      "gt_int16" -> boolInt16 (>)
      "le_int16" -> boolInt16 (<=)
      "ge_int16" -> boolInt16 (>=)
      "eq_int16" -> boolInt16 (==)
      "ne_int16" -> boolInt16 (/=)

      "lt_int32" -> boolInt32 (<)
      "gt_int32" -> boolInt32 (>)
      "le_int32" -> boolInt32 (<=)
      "ge_int32" -> boolInt32 (>=)
      "eq_int32" -> boolInt32 (==)
      "ne_int32" -> boolInt32 (/=)

      "lt_int64" -> boolInt64 (<)
      "gt_int64" -> boolInt64 (>)
      "le_int64" -> boolInt64 (<=)
      "ge_int64" -> boolInt64 (>=)
      "eq_int64" -> boolInt64 (==)
      "ne_int64" -> boolInt64 (/=)

      "neg_uint8" -> unaryUInt8 negate
      "add_uint8" -> binaryUInt8 (+)
      "sub_uint8" -> binaryUInt8 (-)
      "mul_uint8" -> binaryUInt8 (*)
      "div_uint8" -> catchDivideByZero $ binaryUInt8 div
      "mod_uint8" -> catchDivideByZero $ binaryUInt8 mod

      "neg_uint16" -> unaryUInt16 negate
      "add_uint16" -> binaryUInt16 (+)
      "sub_uint16" -> binaryUInt16 (-)
      "mul_uint16" -> binaryUInt16 (*)
      "div_uint16" -> catchDivideByZero $ binaryUInt16 div
      "mod_uint16" -> catchDivideByZero $ binaryUInt16 mod

      "neg_uint32" -> unaryUInt32 negate
      "add_uint32" -> binaryUInt32 (+)
      "sub_uint32" -> binaryUInt32 (-)
      "mul_uint32" -> binaryUInt32 (*)
      "div_uint32" -> catchDivideByZero $ binaryUInt32 div
      "mod_uint32" -> catchDivideByZero $ binaryUInt32 mod

      "neg_uint64" -> unaryUInt64 negate
      "add_uint64" -> binaryUInt64 (+)
      "sub_uint64" -> binaryUInt64 (-)
      "mul_uint64" -> binaryUInt64 (*)
      "div_uint64" -> catchDivideByZero $ binaryUInt64 div
      "mod_uint64" -> catchDivideByZero $ binaryUInt64 mod

      "not_uint8" -> unaryUInt8 complement
      "or_uint8"  -> binaryUInt8 (.|.)
      "and_uint8" -> binaryUInt8 (.&.)
      "xor_uint8" -> binaryUInt8 xor
      "shl_uint8" -> binaryUInt8Int shift
      "rol_uint8" -> binaryUInt8Int rotate

      "not_uint16" -> unaryUInt16 complement
      "or_uint16"  -> binaryUInt16 (.|.)
      "and_uint16" -> binaryUInt16 (.&.)
      "xor_uint16" -> binaryUInt16 xor
      "shl_uint16" -> binaryUInt16Int shift
      "rol_uint16" -> binaryUInt16Int rotate

      "not_uint32" -> unaryUInt32 complement
      "or_uint32"  -> binaryUInt32 (.|.)
      "and_uint32" -> binaryUInt32 (.&.)
      "xor_uint32" -> binaryUInt32 xor
      "shl_uint32" -> binaryUInt32Int shift
      "rol_uint32" -> binaryUInt32Int rotate

      "not_uint64" -> unaryUInt64 complement
      "or_uint64"  -> binaryUInt64 (.|.)
      "and_uint64" -> binaryUInt64 (.&.)
      "xor_uint64" -> binaryUInt64 xor
      "shl_uint64" -> binaryUInt64Int shift
      "rol_uint64" -> binaryUInt64Int rotate

      "lt_uint8" -> boolUInt8 (<)
      "gt_uint8" -> boolUInt8 (>)
      "le_uint8" -> boolUInt8 (<=)
      "ge_uint8" -> boolUInt8 (>=)
      "eq_uint8" -> boolUInt8 (==)
      "ne_uint8" -> boolUInt8 (/=)

      "lt_uint16" -> boolUInt16 (<)
      "gt_uint16" -> boolUInt16 (>)
      "le_uint16" -> boolUInt16 (<=)
      "ge_uint16" -> boolUInt16 (>=)
      "eq_uint16" -> boolUInt16 (==)
      "ne_uint16" -> boolUInt16 (/=)

      "lt_uint32" -> boolUInt32 (<)
      "gt_uint32" -> boolUInt32 (>)
      "le_uint32" -> boolUInt32 (<=)
      "ge_uint32" -> boolUInt32 (>=)
      "eq_uint32" -> boolUInt32 (==)
      "ne_uint32" -> boolUInt32 (/=)

      "lt_uint64" -> boolUInt64 (<)
      "gt_uint64" -> boolUInt64 (>)
      "le_uint64" -> boolUInt64 (<=)
      "ge_uint64" -> boolUInt64 (>=)
      "eq_uint64" -> boolUInt64 (==)
      "ne_uint64" -> boolUInt64 (/=)

      "lt_char" -> boolChar (<)
      "gt_char" -> boolChar (>)
      "le_char" -> boolChar (<=)
      "ge_char" -> boolChar (>=)
      "eq_char" -> boolChar (==)
      "ne_char" -> boolChar (/=)

      "neg_float32" -> unaryFloat32 negate
      "add_float32" -> binaryFloat32 (+)
      "sub_float32" -> binaryFloat32 (-)
      "mul_float32" -> binaryFloat32 (*)
      "div_float32" -> binaryFloat32 (/)
      "mod_float32" -> catchFloatModByZero $ binaryFloat32 mod'

      "neg_float64" -> unaryFloat64 negate
      "add_float64" -> binaryFloat64 (+)
      "sub_float64" -> binaryFloat64 (-)
      "mul_float64" -> binaryFloat64 (*)
      "div_float64" -> binaryFloat64 (/)
      "mod_float64" -> catchFloatModByZero $ binaryFloat64 mod'

      "exp_float32" -> unaryFloat32 exp
      "log_float32" -> unaryFloat32 log
      "sqrt_float32" -> unaryFloat32 sqrt
      "sin_float32" -> unaryFloat32 sin
      "cos_float32" -> unaryFloat32 cos
      "tan_float32" -> unaryFloat32 tan
      "asin_float32" -> unaryFloat32 asin
      "acos_float32" -> unaryFloat32 acos
      "atan_float32" -> unaryFloat32 atan
      "atan2_float32" -> binaryFloat32 atan2
      "sinh_float32" -> unaryFloat32 sinh
      "cosh_float32" -> unaryFloat32 cosh
      "tanh_float32" -> unaryFloat32 tanh
      "asinh_float32" -> unaryFloat32 asinh
      "acosh_float32" -> unaryFloat32 acosh
      "atanh_float32" -> unaryFloat32 atanh
      "trunc_float32" -> unaryFloat32 $ fromInteger . truncate
      "round_float32" -> unaryFloat32 $ fromInteger . round
      "ceil_float32" -> unaryFloat32 $ fromInteger . ceiling
      "floor_float32" -> unaryFloat32 $ fromInteger . floor

      "exp_float64" -> unaryFloat64 exp
      "log_float64" -> unaryFloat64 log
      "sqrt_float64" -> unaryFloat64 sqrt
      "sin_float64" -> unaryFloat64 sin
      "cos_float64" -> unaryFloat64 cos
      "tan_float64" -> unaryFloat64 tan
      "asin_float64" -> unaryFloat64 asin
      "acos_float64" -> unaryFloat64 acos
      "atan_float64" -> unaryFloat64 atan
      "atan2_float64" -> binaryFloat64 atan2
      "sinh_float64" -> unaryFloat64 sinh
      "cosh_float64" -> unaryFloat64 cosh
      "tanh_float64" -> unaryFloat64 tanh
      "asinh_float64" -> unaryFloat64 asinh
      "acosh_float64" -> unaryFloat64 acosh
      "atanh_float64" -> unaryFloat64 atanh
      "trunc_float64" -> unaryFloat64 $ fromInteger . truncate
      "round_float64" -> unaryFloat64 $ fromInteger . round
      "ceil_float64" -> unaryFloat64 $ fromInteger . ceiling
      "floor_float64" -> unaryFloat64 $ fromInteger . floor

      "lt_float32" -> boolFloat32 (<)
      "gt_float32" -> boolFloat32 (>)
      "le_float32" -> boolFloat32 (<=)
      "ge_float32" -> boolFloat32 (>=)
      "eq_float32" -> boolFloat32 (==)
      "ne_float32" -> boolFloat32 (/=)

      "lt_float64" -> boolFloat64 (<)
      "gt_float64" -> boolFloat64 (>)
      "le_float64" -> boolFloat64 (<=)
      "ge_float64" -> boolFloat64 (>=)
      "eq_float64" -> boolFloat64 (==)
      "ne_float64" -> boolFloat64 (/=)

      "show_int8" -> showInteger (show :: Int8 -> String)
      "show_int16" -> showInteger (show :: Int16 -> String)
      "show_int32" -> showInteger (show :: Int32 -> String)
      "show_int64" -> showInteger (show :: Int64 -> String)
      "show_uint8" -> showInteger (show :: Word8 -> String)
      "show_uint16" -> showInteger (show :: Word16 -> String)
      "show_uint32" -> showInteger (show :: Word32 -> String)
      "show_uint64" -> showInteger (show :: Word64 -> String)
      "show_float32" -> showFloat (show :: Float -> String)
      "show_float64" -> showFloat (show :: Double -> String)

      "empty" -> do
        Array xs ::: r <- readIORef stackRef
        writeIORef stackRef r
        if Vector.null xs
          then word callStack (Qualified Vocabulary.global "true") []
          else word callStack (Qualified Vocabulary.global "false") []

      "head" -> do
        Array xs ::: r <- readIORef stackRef
        if Vector.null xs
          then do
            writeIORef stackRef r
            word callStack (Qualified Vocabulary.global "none") []
          else do
            let x = xs ! 0
            writeIORef stackRef $ x ::: r
            -- FIXME: Use right args.
            word callStack (Qualified Vocabulary.global "some") []

      "last" -> do
        Array xs ::: r <- readIORef stackRef
        if Vector.null xs
          then do
            writeIORef stackRef r
            word callStack (Qualified Vocabulary.global "none") []
          else do
            let x = xs ! (Vector.length xs - 1)
            writeIORef stackRef $ x ::: r
            -- FIXME: Use right args.
            word callStack (Qualified Vocabulary.global "some") []

      "append" -> do
        x ::: Array xs ::: r <- readIORef stackRef
        writeIORef stackRef $ Array (Vector.snoc xs x) ::: r
      "prepend" -> do
        Array xs ::: x ::: r <- readIORef stackRef
        writeIORef stackRef $ Array (Vector.cons x xs) ::: r
      "cat" -> do
        Array ys ::: Array xs ::: r <- readIORef stackRef
        writeIORef stackRef $ Array (xs <> ys) ::: r
      "get" -> do
        Integer i _ ::: Array xs ::: r <- readIORef stackRef
        if i < 0 || i >= fromIntegral (length xs)
          then do
            writeIORef stackRef r
            word callStack (Qualified Vocabulary.global "none") []
          else do
            writeIORef stackRef $ (xs ! fromIntegral i) ::: r
            -- FIXME: Use right args.
            word callStack (Qualified Vocabulary.global "some") []
      "set" -> do
        Integer i _ ::: x ::: Array xs ::: r <- readIORef stackRef
        if i < 0 || i >= fromIntegral (length xs)
          then do
            writeIORef stackRef r
            word callStack (Qualified Vocabulary.global "none") []
          else do
            let (before, after) = Vector.splitAt (fromIntegral i) xs
            writeIORef stackRef $ Array
              (before <> Vector.singleton x <> Vector.tail after) ::: r
            -- FIXME: Use right args.
            word callStack (Qualified Vocabulary.global "some") []
      "print" -> do
        Array cs ::: r <- readIORef stackRef
        writeIORef stackRef r
        mapM_ (\ (Character c) -> hPutChar stdout' c) cs
      "get_line" -> do
        line <- hGetLine stdin'
        modifyIORef' stackRef (Array (Vector.fromList (map Character line)) :::)
      "flush_stdout" -> hFlush stdout'

      "read_file" -> do
        Array cs ::: r <- readIORef stackRef
        contents <- catchFileAccessErrors $
          readFile $ map (\ (Character c) -> c) $ Vector.toList cs
        writeIORef stackRef $ Array
          (Vector.fromList (map Character contents)) ::: r
      "write_file" -> do
        Array cs ::: Array bs ::: r <- readIORef stackRef
        writeIORef stackRef r
        catchFileAccessErrors $
          writeFile (map (\ (Character c) -> c) $ Vector.toList cs) $
          map (\ (Character c) -> c) $ Vector.toList bs
      "append_file" -> do
        Array cs ::: Array bs ::: r <- readIORef stackRef
        writeIORef stackRef r
        catchFileAccessErrors $
          appendFile (map (\ (Character c) -> c) $ Vector.toList cs) $
          map (\ (Character c) -> c) $ Vector.toList bs

      "tail" -> do
        Array xs ::: r <- readIORef stackRef
        if Vector.null xs
          then do
            writeIORef stackRef r
            word callStack (Qualified Vocabulary.global "none") []
          else do
            let xs' = Vector.tail xs
            writeIORef stackRef $ Array xs' ::: r
            -- FIXME: Use right args.
            word callStack (Qualified Vocabulary.global "some") []

      "init" -> do
        Array xs ::: r <- readIORef stackRef
        if Vector.null xs
          then do
            writeIORef stackRef r
            word callStack (Qualified Vocabulary.global "none") []
          else do
            let xs' = Vector.init xs
            writeIORef stackRef $ Array xs' ::: r
            -- FIXME: Use right args.
            word callStack (Qualified Vocabulary.global "some") []

      "draw" -> do
        Array ys ::: rest <- readIORef stackRef
        writeIORef stackRef rest
        let
          height = length ys
          width = if null ys
            then 0
            else case ys ! 0 of
              Array xs -> Vector.length xs
              _ -> error "draw: the typechecker has failed us (rows)"
        hPrintf stdout' "\ESC]1337;File=width=%dpx;height=%dpx;inline=1:%s\BEL\n"
          width height
          $ map ((toEnum :: Int -> Char) . fromIntegral)
          $ ByteString.unpack $ Base64.encode
          $ LazyByteString.toStrict $ Png.encodePng
          $ Picture.generateImage (\ x y -> case ys ! y of
            Array xs -> case xs ! x of
              Algebraic _ channels -> case channels of
                [Integer r _, Integer g _, Integer b _, Integer a _]
                  -> Picture.PixelRGBA8
                    (fromIntegral r)
                    (fromIntegral g)
                    (fromIntegral b)
                    (fromIntegral a)
                _ -> error "draw: the typechecker has failed us (channel)"
              _ -> error "draw: the typechecker has failed us (column)"
            _ -> error "draw: the typechecker has failed us (row)")
            width height
      _ -> error "no such intrinsic"

      where

      unaryInt8 :: (Int8 -> Int8) -> IO ()
      unaryInt8 f = do
        Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f $ fromIntegral x
        writeIORef stackRef $ Integer result Signed8 ::: r

      binaryInt8 :: (Int8 -> Int8 -> Int8) -> IO ()
      binaryInt8 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Signed8 ::: r

      binaryInt8Int :: (Int8 -> Int -> Int8) -> IO ()
      binaryInt8Int f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Signed8 ::: r

      unaryInt16 :: (Int16 -> Int16) -> IO ()
      unaryInt16 f = do
        Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f $ fromIntegral x
        writeIORef stackRef $ Integer result Signed16 ::: r

      binaryInt16 :: (Int16 -> Int16 -> Int16) -> IO ()
      binaryInt16 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Signed16 ::: r

      binaryInt16Int :: (Int16 -> Int -> Int16) -> IO ()
      binaryInt16Int f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Signed16 ::: r

      unaryInt32 :: (Int32 -> Int32) -> IO ()
      unaryInt32 f = do
        Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f $ fromIntegral x
        writeIORef stackRef $ Integer result Signed32 ::: r

      binaryInt32 :: (Int32 -> Int32 -> Int32) -> IO ()
      binaryInt32 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Signed32 ::: r

      binaryInt32Int :: (Int32 -> Int -> Int32) -> IO ()
      binaryInt32Int f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Signed32 ::: r

      unaryInt64 :: (Int64 -> Int64) -> IO ()
      unaryInt64 f = do
        Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f $ fromIntegral x
        writeIORef stackRef $ Integer result Signed64 ::: r

      binaryInt64 :: (Int64 -> Int64 -> Int64) -> IO ()
      binaryInt64 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Signed64 ::: r

      binaryInt64Int :: (Int64 -> Int -> Int64) -> IO ()
      binaryInt64Int f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Signed64 ::: r

      unaryUInt8 :: (Word8 -> Word8) -> IO ()
      unaryUInt8 f = do
        Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f $ fromIntegral x
        writeIORef stackRef $ Integer result Unsigned8 ::: r

      binaryUInt8 :: (Word8 -> Word8 -> Word8) -> IO ()
      binaryUInt8 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Unsigned8 ::: r

      binaryUInt8Int :: (Word8 -> Int -> Word8) -> IO ()
      binaryUInt8Int f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Unsigned8 ::: r

      unaryUInt16 :: (Word16 -> Word16) -> IO ()
      unaryUInt16 f = do
        Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f $ fromIntegral x
        writeIORef stackRef $ Integer result Unsigned16 ::: r

      binaryUInt16 :: (Word16 -> Word16 -> Word16) -> IO ()
      binaryUInt16 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Unsigned16 ::: r

      binaryUInt16Int :: (Word16 -> Int -> Word16) -> IO ()
      binaryUInt16Int f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Unsigned16 ::: r

      unaryUInt32 :: (Word32 -> Word32) -> IO ()
      unaryUInt32 f = do
        Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f $ fromIntegral x
        writeIORef stackRef $ Integer result Unsigned32 ::: r

      binaryUInt32 :: (Word32 -> Word32 -> Word32) -> IO ()
      binaryUInt32 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Unsigned32 ::: r

      binaryUInt32Int :: (Word32 -> Int -> Word32) -> IO ()
      binaryUInt32Int f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Unsigned32 ::: r

      unaryUInt64 :: (Word64 -> Word64) -> IO ()
      unaryUInt64 f = do
        Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f $ fromIntegral x
        writeIORef stackRef $ Integer result Unsigned64 ::: r

      binaryUInt64 :: (Word64 -> Word64 -> Word64) -> IO ()
      binaryUInt64 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Unsigned64 ::: r

      binaryUInt64Int :: (Word64 -> Int -> Word64) -> IO ()
      binaryUInt64Int f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromIntegral $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Integer result Unsigned64 ::: r

      boolInt8 :: (Int8 -> Int8 -> Bool) -> IO ()
      boolInt8 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      boolInt16 :: (Int16 -> Int16 -> Bool) -> IO ()
      boolInt16 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      boolInt32 :: (Int32 -> Int32 -> Bool) -> IO ()
      boolInt32 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      boolInt64 :: (Int64 -> Int64 -> Bool) -> IO ()
      boolInt64 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      boolUInt8 :: (Word8 -> Word8 -> Bool) -> IO ()
      boolUInt8 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      boolUInt16 :: (Word16 -> Word16 -> Bool) -> IO ()
      boolUInt16 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      boolUInt32 :: (Word32 -> Word32 -> Bool) -> IO ()
      boolUInt32 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      boolUInt64 :: (Word64 -> Word64 -> Bool) -> IO ()
      boolUInt64 f = do
        Integer y _ ::: Integer x _ ::: r <- readIORef stackRef
        let !result = fromEnum $ f (fromIntegral x) (fromIntegral y)
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      boolChar :: (Char -> Char -> Bool) -> IO ()
      boolChar f = do
        Character y ::: Character x ::: r <- readIORef stackRef
        let !result = fromEnum $ f x y
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      unaryFloat32 :: (Float -> Float) -> IO ()
      unaryFloat32 f = do
        Float x _ ::: r <- readIORef stackRef
        let !result = realToFrac $ f $ realToFrac x
        writeIORef stackRef $ Float result Float32 ::: r

      binaryFloat32 :: (Float -> Float -> Float) -> IO ()
      binaryFloat32 f = do
        Float y _ ::: Float x _ ::: r <- readIORef stackRef
        let !result = realToFrac $ f (realToFrac x) (realToFrac y)
        writeIORef stackRef $ Float result Float32 ::: r

      boolFloat32 :: (Float -> Float -> Bool) -> IO ()
      boolFloat32 f = do
        Float y _ ::: Float x _ ::: r <- readIORef stackRef
        let !result = fromEnum $ f (realToFrac x) (realToFrac y)
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      unaryFloat64 :: (Double -> Double) -> IO ()
      unaryFloat64 f = do
        Float x _ ::: r <- readIORef stackRef
        let !result = realToFrac $ f $ realToFrac x
        writeIORef stackRef $ Float result Float64 ::: r

      binaryFloat64 :: (Double -> Double -> Double) -> IO ()
      binaryFloat64 f = do
        Float y _ ::: Float x _ ::: r <- readIORef stackRef
        let !result = f x y
        writeIORef stackRef $ Float result Float64 ::: r

      boolFloat64 :: (Double -> Double -> Bool) -> IO ()
      boolFloat64 f = do
        Float y _ ::: Float x _ ::: r <- readIORef stackRef
        let !result = fromEnum $ f x y
        writeIORef stackRef $ Algebraic (ConstructorIndex result) [] ::: r

      showInteger :: Num a => (a -> String) -> IO ()
      showInteger f = do
        Integer x _ ::: r <- readIORef stackRef
        writeIORef stackRef $ Array
          (Vector.fromList $ map Character $ f (fromIntegral x)) ::: r

      showFloat :: Fractional a => (a -> String) -> IO ()
      showFloat f = do
        Float x _ ::: r <- readIORef stackRef
        writeIORef stackRef $ Array
          (Vector.fromList $ map Character $ f $ realToFrac x) ::: r

      catchDivideByZero :: IO a -> IO a
      catchDivideByZero action = action `catch` \ e -> case e of
        DivideByZero -> throwIO $ Failure $ Pretty.vcat
          $ "Execution failure: integer division by zero"
          : "Call stack:"
          : map (Pretty.nest 4 . pPrint) callStack
        _ -> throwIO e

      catchFloatModByZero :: IO a -> IO a
      catchFloatModByZero action = action `catch` \ e -> case e of
        RatioZeroDenominator -> throwIO $ Failure $ Pretty.vcat
          $ "Execution failure: float modulus by zero"
          : "Call stack:"
          : map (Pretty.nest 4 . pPrint) callStack
        _ -> throwIO e

      catchFileAccessErrors :: IO a -> IO a
      catchFileAccessErrors action = action `catch` \ e ->
        throwIO $ Failure $ Pretty.vcat
        $ (if isAlreadyInUseError e then
            "Execution failure: file already in use"
          else if isDoesNotExistError e then
            "Execution failure: file does not exist"
          else if isPermissionError e then
            "Execution failure: not permitted to open file"
          else
            "Execution failure: unknown file access error")
        : "Call stack:"
        : map (Pretty.nest 4 . pPrint) callStack

  let entryPointName = fromMaybe mainName mName
  word [entryPointName] entryPointName mainArgs
  toList <$> readIORef stackRef

data Failure = Failure Pretty.Doc
  deriving (Typeable)

instance Exception Failure

instance Show Failure where
  show (Failure message) = Pretty.render message
