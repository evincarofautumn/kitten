{-# LANGUAGE OverloadedStrings #-}

module Kitten.Interpret
  ( Failure
  , interpret
  ) where

import Control.Exception (Exception, throwIO)
import Data.Fixed (mod')
import Data.Foldable (forM_)
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Data.Vector (Vector, (!))
import Data.Word
import Kitten.Bits
import Kitten.Definition (mainName)
import Kitten.Dictionary (Dictionary)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Monad (runKitten)
import Kitten.Name
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Type(..))
import System.IO (Handle, hPutChar, hPutStrLn)
import Text.Printf (hPrintf)
import qualified Codec.Picture.Png as Png
import qualified Codec.Picture.Types as Picture
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Vector as Vector
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
  -> Handle
  -> Handle
  -> Handle
  -> [Value Type]
  -> IO [Value Type]
interpret dictionary mName mainArgs stdin stdout stderr initialStack = do
  -- TODO: Types.
  stackRef <- newIORef initialStack
  localsRef <- newIORef []
  currentClosureRef <- newIORef []
  let

    word :: Qualified -> [Type] -> IO ()
    word name args = do
      let mangled = Instantiated name args
      case Dictionary.lookup mangled dictionary of
        -- An entry in the dictionary should already be instantiated, so we
        -- shouldn't need to instantiate it again here.
        Just (Entry.Word _ _ _ _ _ (Just body)) -> term body
        _ -> case Dictionary.lookup (Instantiated name []) dictionary of
          -- A regular word.
          Just (Entry.Word _ _ _ _ _ (Just body)) -> do
            mBody' <- runKitten $ Instantiate.term TypeEnv.empty body args
            case mBody' of
              Right body' -> term body'
              Left reports -> do
                hPutStrLn stdout $ Pretty.render $ Pretty.vcat
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
              <- Dictionary.lookup (Instantiated name []) dictionary
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
        writeIORef stackRef (Closure name (reverse closure) : r')
      NewVector _ size _ -> do
        r <- readIORef stackRef
        let (values, r') = splitAt size r
        writeIORef stackRef
          (Array (Vector.reverse $ Vector.fromList values) : r')
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
        ((Array $ fmap Character $ Vector.fromList $ Text.unpack text) :)
      _ -> modifyIORef' stackRef (value :)

    intrinsic :: Unqualified -> IO ()
    intrinsic name = case name of
      "abort" -> do
        (Array cs : r) <- readIORef stackRef
        writeIORef stackRef r
        let message = map (\ (Character c) -> c) $ Vector.toList cs
        throwIO $ Failure $ Pretty.hsep
          [ "Execution failure:"
          , Pretty.text message
          ]

      "neg_int8" -> unaryInt8 negate
      "add_int8" -> binaryInt8 (+)
      "sub_int8" -> binaryInt8 (-)
      "mul_int8" -> binaryInt8 (*)
      "div_int8" -> binaryInt8 div
      "mod_int8" -> binaryInt8 mod

      "neg_int16" -> unaryInt16 negate
      "add_int16" -> binaryInt16 (+)
      "sub_int16" -> binaryInt16 (-)
      "mul_int16" -> binaryInt16 (*)
      "div_int16" -> binaryInt16 div
      "mod_int16" -> binaryInt16 mod

      "neg_int32" -> unaryInt32 negate
      "add_int32" -> binaryInt32 (+)
      "sub_int32" -> binaryInt32 (-)
      "mul_int32" -> binaryInt32 (*)
      "div_int32" -> binaryInt32 div
      "mod_int32" -> binaryInt32 mod

      "neg_int64" -> unaryInt64 negate
      "add_int64" -> binaryInt64 (+)
      "sub_int64" -> binaryInt64 (-)
      "mul_int64" -> binaryInt64 (*)
      "div_int64" -> binaryInt64 div
      "mod_int64" -> binaryInt64 mod

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
      "div_uint8" -> binaryUInt8 div
      "mod_uint8" -> binaryUInt8 mod

      "neg_uint16" -> unaryUInt16 negate
      "add_uint16" -> binaryUInt16 (+)
      "sub_uint16" -> binaryUInt16 (-)
      "mul_uint16" -> binaryUInt16 (*)
      "div_uint16" -> binaryUInt16 div
      "mod_uint16" -> binaryUInt16 mod

      "neg_uint32" -> unaryUInt32 negate
      "add_uint32" -> binaryUInt32 (+)
      "sub_uint32" -> binaryUInt32 (-)
      "mul_uint32" -> binaryUInt32 (*)
      "div_uint32" -> binaryUInt32 div
      "mod_uint32" -> binaryUInt32 mod

      "neg_uint64" -> unaryUInt64 negate
      "add_uint64" -> binaryUInt64 (+)
      "sub_uint64" -> binaryUInt64 (-)
      "mul_uint64" -> binaryUInt64 (*)
      "div_uint64" -> binaryUInt64 div
      "mod_uint64" -> binaryUInt64 mod

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

      "neg_float32" -> unaryFloat32 negate
      "add_float32" -> binaryFloat32 (+)
      "sub_float32" -> binaryFloat32 (-)
      "mul_float32" -> binaryFloat32 (*)
      "div_float32" -> binaryFloat32 (/)
      "mod_float32" -> binaryFloat32 mod'

      "neg_float64" -> unaryFloat64 negate
      "add_float64" -> binaryFloat64 (+)
      "sub_float64" -> binaryFloat64 (-)
      "mul_float64" -> binaryFloat64 (*)
      "div_float64" -> binaryFloat64 (/)
      "mod_float64" -> binaryFloat64 mod'

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

      "empty" -> do
        (Array xs : r) <- readIORef stackRef
        writeIORef stackRef r
        if Vector.null xs
          then word (Qualified Vocabulary.global "true") []
          else word (Qualified Vocabulary.global "false") []
      "head" -> do
        (Array xs : r) <- readIORef stackRef
        if Vector.null xs
          then do
            writeIORef stackRef r
            word (Qualified Vocabulary.global "none") []
          else do
            let x = xs ! 0
            writeIORef stackRef $ x : r
            -- FIXME: Use right args.
            word (Qualified Vocabulary.global "some") []
      "append" -> do
        (x : Array xs : r) <- readIORef stackRef
        writeIORef stackRef $ Array (Vector.snoc xs x) : r
      "prepend" -> do
        (Array xs : x : r) <- readIORef stackRef
        writeIORef stackRef $ Array (Vector.cons x xs) : r
      "cat" -> do
        (Array ys : Array xs : r) <- readIORef stackRef
        writeIORef stackRef $ Array (xs <> ys) : r
      "get" -> do
        (Integer i _ : Array xs : r) <- readIORef stackRef
        if i < 0 || i >= fromIntegral (length xs)
          then do
            writeIORef stackRef r
            word (Qualified Vocabulary.global "none") []
          else do
            writeIORef stackRef $ (xs ! fromIntegral i) : r
            -- FIXME: Use right args.
            word (Qualified Vocabulary.global "some") []
      "set" -> do
        (Integer i _ : x : Array xs : r) <- readIORef stackRef
        if i < 0 || i >= fromIntegral (length xs)
          then do
            writeIORef stackRef r
            word (Qualified Vocabulary.global "none") []
          else do
            let (before, after) = Vector.splitAt (fromIntegral i) xs
            writeIORef stackRef $ Array
              (before <> Vector.singleton x <> Vector.tail after) : r
            -- FIXME: Use right args.
            word (Qualified Vocabulary.global "some") []
      "print" -> do
        (Array cs : r) <- readIORef stackRef
        writeIORef stackRef r
        mapM_ (\ (Character c) -> hPutChar stdout c) cs
      "tail" -> do
        (Array xs : r) <- readIORef stackRef
        if Vector.null xs
          then do
            writeIORef stackRef r
            word (Qualified Vocabulary.global "none") []
          else do
            let xs' = Vector.tail xs
            writeIORef stackRef $ Array xs' : r
            -- FIXME: Use right args.
            word (Qualified Vocabulary.global "some") []

      "draw" -> do
        (Array ys : rest) <- readIORef stackRef
        writeIORef stackRef rest
        let
          height = length ys
          width = if null ys
            then 0
            else case ys ! 0 of
              Array xs -> Vector.length xs
        hPrintf stdout "\ESC]1337;File=width=%dpx;height=%dpx;inline=1:%s\BEL\n"
          width height
          $ map ((toEnum :: Int -> Char) . fromIntegral)
          $ ByteString.unpack $ Base64.encode
          $ LazyByteString.toStrict $ Png.encodePng
          $ Picture.generateImage (\ x y -> case (ys ! y) of
            Array xs -> case xs ! x of
              Algebraic _ channels -> case channels of
                [Integer r _, Integer g _, Integer b _, Integer a _]
                  -> Picture.PixelRGBA8
                    (fromIntegral r)
                    (fromIntegral g)
                    (fromIntegral b)
                    (fromIntegral a))
            width height
      _ -> error "no such intrinsic"

      where

      unaryInt8 :: (Int8 -> Int8) -> IO ()
      unaryInt8 f = do
        (Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral $ f (fromIntegral x)) Signed8
          : r

      binaryInt8 :: (Int8 -> Int8 -> Int8) -> IO ()
      binaryInt8 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral
            $ f (fromIntegral x) (fromIntegral y)) Signed8
          : r

      unaryInt16 :: (Int16 -> Int16) -> IO ()
      unaryInt16 f = do
        (Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral $ f (fromIntegral x)) Signed16
          : r

      binaryInt16 :: (Int16 -> Int16 -> Int16) -> IO ()
      binaryInt16 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral
            $ f (fromIntegral x) (fromIntegral y)) Signed16
          : r

      unaryInt32 :: (Int32 -> Int32) -> IO ()
      unaryInt32 f = do
        (Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral $ f (fromIntegral x)) Signed32
          : r

      binaryInt32 :: (Int32 -> Int32 -> Int32) -> IO ()
      binaryInt32 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral
            $ f (fromIntegral x) (fromIntegral y)) Signed32
          : r

      unaryInt64 :: (Int64 -> Int64) -> IO ()
      unaryInt64 f = do
        (Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral $ f (fromIntegral x)) Signed64
          : r

      binaryInt64 :: (Int64 -> Int64 -> Int64) -> IO ()
      binaryInt64 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral
            $ f (fromIntegral x) (fromIntegral y)) Signed64
          : r

      unaryUInt8 :: (Word8 -> Word8) -> IO ()
      unaryUInt8 f = do
        (Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral $ f (fromIntegral x)) Unsigned8
          : r

      binaryUInt8 :: (Word8 -> Word8 -> Word8) -> IO ()
      binaryUInt8 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral
            $ f (fromIntegral x) (fromIntegral y)) Unsigned8
          : r

      unaryUInt16 :: (Word16 -> Word16) -> IO ()
      unaryUInt16 f = do
        (Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral $ f (fromIntegral x)) Unsigned16
          : r

      binaryUInt16 :: (Word16 -> Word16 -> Word16) -> IO ()
      binaryUInt16 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral
            $ f (fromIntegral x) (fromIntegral y)) Unsigned16
          : r

      unaryUInt32 :: (Word32 -> Word32) -> IO ()
      unaryUInt32 f = do
        (Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral $ f (fromIntegral x)) Unsigned32
          : r

      binaryUInt32 :: (Word32 -> Word32 -> Word32) -> IO ()
      binaryUInt32 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral
            $ f (fromIntegral x) (fromIntegral y)) Unsigned32
          : r

      unaryUInt64 :: (Word64 -> Word64) -> IO ()
      unaryUInt64 f = do
        (Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral $ f (fromIntegral x)) Unsigned64
          : r

      binaryUInt64 :: (Word64 -> Word64 -> Word64) -> IO ()
      binaryUInt64 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Integer (fromIntegral
            $ f (fromIntegral x) (fromIntegral y)) Unsigned64
          : r

      boolInt8 :: (Int8 -> Int8 -> Bool) -> IO ()
      boolInt8 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Algebraic (ConstructorIndex $ fromEnum
            $ f (fromIntegral x) (fromIntegral y)) []
          : r

      boolInt16 :: (Int16 -> Int16 -> Bool) -> IO ()
      boolInt16 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Algebraic (ConstructorIndex $ fromEnum
            $ f (fromIntegral x) (fromIntegral y)) []
          : r

      boolInt32 :: (Int32 -> Int32 -> Bool) -> IO ()
      boolInt32 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Algebraic (ConstructorIndex $ fromEnum
            $ f (fromIntegral x) (fromIntegral y)) []
          : r

      boolInt64 :: (Int64 -> Int64 -> Bool) -> IO ()
      boolInt64 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Algebraic (ConstructorIndex $ fromEnum
            $ f (fromIntegral x) (fromIntegral y)) []
          : r

      boolUInt8 :: (Word8 -> Word8 -> Bool) -> IO ()
      boolUInt8 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Algebraic (ConstructorIndex $ fromEnum
            $ f (fromIntegral x) (fromIntegral y)) []
          : r

      boolUInt16 :: (Word16 -> Word16 -> Bool) -> IO ()
      boolUInt16 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Algebraic (ConstructorIndex $ fromEnum
            $ f (fromIntegral x) (fromIntegral y)) []
          : r

      boolUInt32 :: (Word32 -> Word32 -> Bool) -> IO ()
      boolUInt32 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Algebraic (ConstructorIndex $ fromEnum
            $ f (fromIntegral x) (fromIntegral y)) []
          : r

      boolUInt64 :: (Word64 -> Word64 -> Bool) -> IO ()
      boolUInt64 f = do
        (Integer y _ : Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef
          $ Algebraic (ConstructorIndex $ fromEnum
            $ f (fromIntegral x) (fromIntegral y)) []
          : r

      unaryFloat32 :: (Float -> Float) -> IO ()
      unaryFloat32 f = do
        (Float x _ : r) <- readIORef stackRef
        writeIORef stackRef $ Float
          (realToFrac (f (realToFrac x))) Float32 : r

      binaryFloat32 :: (Float -> Float -> Float) -> IO ()
      binaryFloat32 f = do
        (Float y _ : Float x _ : r) <- readIORef stackRef
        writeIORef stackRef $ Float
          (realToFrac (f (realToFrac x) (realToFrac y))) Float32 : r

      boolFloat32 :: (Float -> Float -> Bool) -> IO ()
      boolFloat32 f = do
        (Float y _ : Float x _ : r) <- readIORef stackRef
        writeIORef stackRef $ Algebraic
          (ConstructorIndex $ fromEnum $ f (realToFrac x) (realToFrac y))
          [] : r

      unaryFloat64 :: (Double -> Double) -> IO ()
      unaryFloat64 f = do
        (Float x _ : r) <- readIORef stackRef
        writeIORef stackRef $ Float
          (realToFrac (f (realToFrac x))) Float64 : r

      binaryFloat64 :: (Double -> Double -> Double) -> IO ()
      binaryFloat64 f = do
        (Float y _ : Float x _ : r) <- readIORef stackRef
        writeIORef stackRef $ Float (f x y) Float64 : r

      boolFloat64 :: (Double -> Double -> Bool) -> IO ()
      boolFloat64 f = do
        (Float y _ : Float x _ : r) <- readIORef stackRef
        writeIORef stackRef $ Algebraic
          (ConstructorIndex $ fromEnum $ f x y) [] : r

      showInteger :: Num a => (a -> String) -> IO ()
      showInteger f = do
        (Integer x _ : r) <- readIORef stackRef
        writeIORef stackRef $ Array
          (Vector.fromList $ map Character $ f (fromIntegral x)) : r

  word (fromMaybe mainName mName) mainArgs
  readIORef stackRef

data Failure = Failure Pretty.Doc
  deriving (Typeable)

instance Exception Failure

instance Show Failure where
  show (Failure message) = Pretty.render message
