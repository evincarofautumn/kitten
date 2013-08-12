{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Kitten.Util.Text
  ( Textable(..)
  , ToText(..)
  , showText
  , module Data.Text
  ) where

import Data.Text

data Textable = forall a. (ToText a) => Textable a

instance ToText Textable where
  toText (Textable x) = toText x

class ToText a where
  toText :: a -> Text

showText :: (Show a) => a -> Text
showText = pack . show
