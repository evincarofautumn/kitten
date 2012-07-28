{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text (module T, Text.show, toText, (+++), (<++>)) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Data.Function
import Data.Text as T
import Prelude as P

class Concatenable a where
  toText :: a -> T.Text

instance Concatenable Char     where toText = T.singleton
instance Concatenable T.Text   where toText = P.id
instance Concatenable String   where toText = T.pack
instance Concatenable [T.Text] where toText = T.concat
instance Concatenable [String] where toText = T.concat . P.map T.pack

(<++>)
  :: (Concatenable a, Concatenable b, Applicative f)
  => f a
  -> f b
  -> f T.Text
f <++> g = (T.append `on` toText) <$> f <*> g

(+++)
  :: T.Text
  -> T.Text
  -> T.Text
x +++ y = T.append x y

show
  :: (Show a)
  => a
  -> T.Text
show x = T.pack $ P.show x
