{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Kitten.Util.Text
  ( Textable(..)
  , ToText(..)
  , readFileUtf8
  , showText
  , module Data.Text
  ) where

import Data.Text
import Data.Text.Encoding

import qualified Data.ByteString as B

data Textable = forall a. (ToText a) => Textable a

instance ToText Textable where
  toText (Textable x) = toText x

class ToText a where
  toText :: a -> Text

instance ToText Text where
  toText = pack . show

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = decodeUtf8 <$> B.readFile path

showText :: (Show a) => a -> Text
showText = pack . show
