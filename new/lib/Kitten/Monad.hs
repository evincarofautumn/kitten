module Kitten.Monad
  ( K
  , KT
  , attempt
  , runKitten
  ) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class
import Kitten.Informer (Informer(..))
import Kitten.Origin (Origin)
import Kitten.Report (Report(..))
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Text.PrettyPrint as Pretty

newtype KT m a = KT
  { unKT :: Context -> Reports -> m (Either Reports (a, Reports)) }

type Context = [(Origin, Pretty.Doc)]

type Reports = [Report]

type K = KT IO

attempt :: (Monad m) => KT m a -> KT m Bool
attempt action = KT $ \ context reports -> do
  mr <- unKT action context reports
  return $ Right $ case mr of
    Left reports' -> (False, reports')
    Right (_, reports') -> (True, reports')

runKitten :: (Monad m) => KT m a -> m (Either [Report] a)
runKitten (KT m) = do
  mr <- m [] []
  return $ case mr of
    Left reports -> Left reports
    Right (result, _) -> Right result

instance (Monad m) => Functor (KT m) where
  fmap f (KT ax) = KT $ \ context reports -> do
    mr <- ax context reports
    case mr of
      Left reports' -> return $ Left reports'
      Right (x, reports') -> return $ Right (f x, reports')

instance (Monad m) => Applicative (KT m) where
  pure x = KT $ \ _context reports -> return $ Right (x, reports)
  KT af <*> KT ax = KT $ \ context reports -> do
    mf <- af context reports
    case mf of
      Right (f, reports') -> do
        mx <- ax context reports'
        case mx of
          Right (x, reports'') -> return $ Right (f x, reports'')
          Left reports'' -> return $ Left reports''
      Left reports' -> return $ Left reports'

instance (Monad m) => Monad (KT m) where
  return x = KT $ \ _context reports -> return $ Right (x, reports)
  KT ax >>= f = KT $ \ context reports -> do
    mx <- ax context reports
    case mx of
      Left reports' -> return $ Left reports'
      Right (x, reports') -> unKT (f x) context reports'
  fail = error "do not use 'fail'"

instance (MonadIO m) => MonadFix (KT m) where
  mfix k = KT $ \ context reports -> do
    m <- liftIO newEmptyMVar
    a <- liftIO $ unsafeInterleaveIO $ takeMVar m
    mx <- unKT (k a) context reports
    case mx of
      Left{} -> return mx
      Right (x, _) -> do
        liftIO $ putMVar m x
        return mx

instance (MonadIO m) => MonadIO (KT m) where
  liftIO m = KT $ \ _context reports -> do
    x <- liftIO m
    return $ Right (x, reports)

instance (Monad m) => Informer (KT m) where
  checkpoint = KT $ \ _context reports -> return
    $ if null reports then Right ((), reports) else Left reports
  halt = KT $ \ _context reports -> return $ Left reports
  report r = KT $ \ context reports -> return . Right . (,) () $ case context of
    [] -> r : reports
    _ -> Context context r : reports
  while origin message action = KT $ \ context reports
    -> unKT action ((origin, message) : context) reports
