import           Control.Applicative (liftA2)

newtype ReaderT r m a = ReaderT
  { runReaderT :: (r -> m a)
  }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ fmap f . rma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ (\_ -> pure a)
  (<*>) (ReaderT rmf) (ReaderT rma) = ReaderT $ \r -> (rmf r) <*> (rma r)

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) (ReaderT rma) f = ReaderT (\r -> (rma r) >>= (\a -> runReaderT (f a) r))
