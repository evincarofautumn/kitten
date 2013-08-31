{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kitten.Util.FailWriter
  ( FailWriterT
  , guardLiftM2
  , guardMapM
  , guardReturn
  , guardSequence
  , halt
  , runFailWriterT
  , throwMany
  ) where

import Control.Applicative (Applicative)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Data.Monoid
import Data.Traversable (Traversable)

import qualified Data.Traversable as T

newtype FailWriterT w m a = FailWriterT
  (MaybeT (WriterT w m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Monoid w) => MonadTrans (FailWriterT w) where
  lift = FailWriterT . lift . lift

guardLiftM2
  :: (Monad m, Monoid w)
  => (a -> b -> c)
  -> FailWriterT w m a
  -> FailWriterT w m b
  -> FailWriterT w m c
guardLiftM2 f ma mb = liftM2 (,)
  (guardReturn ma) (guardReturn mb)
  >>= \ (mA, mB) -> case (mA, mB) of
    (Just a, Just b) -> return $ f a b
    _ -> halt

guardMapM
  :: (Monad m, Monoid w, Traversable t)
  => (a -> FailWriterT w m b)
  -> t a
  -> FailWriterT w m (t b)
guardMapM f xs = guardSequence (fmap f xs)

-- | Accumulates errors and continues.
guardReturn
  :: (Monad m, Monoid w)
  => FailWriterT w m a
  -> FailWriterT w m (Maybe a)
guardReturn (FailWriterT m)
  = FailWriterT . lift $ runMaybeT m

guardSequence
  :: (Monad m, Monoid w, Traversable t)
  => t (FailWriterT w m a)
  -> FailWriterT w m (t a)
guardSequence xs
  = liftM T.sequence (T.mapM guardReturn xs)
  >>= maybe halt return

halt
  :: (Monad m, Monoid w)
  => FailWriterT w m a
halt = FailWriterT $ MaybeT (return Nothing)

runFailWriterT
  :: (Monad m, Monoid w)
  => (w -> Bool)  -- ^ If true, return 'Right'.
  -> FailWriterT w m a
  -> m (Either w a)
runFailWriterT isFailure (FailWriterT m)
  = liftM f . runWriterT $ runMaybeT m
  where
  f (Just result, errors)
    | isFailure errors = Right result
  f (_, errors) = Left errors

throwMany
  :: (Monad m, Monoid w)
  => w
  -> FailWriterT w m a
throwMany xs = do
  FailWriterT . lift $ tell xs
  halt
