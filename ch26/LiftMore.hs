import Control.Monad.Trans
import Control.Applicative (liftA2)
import Control.Monad.IO.Class
import Control.Monad (liftM)

--------------------------------------------------------------------------------
newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

instance MonadTrans (EitherT a) where
    lift = EitherT . (liftM Right)

--------------------------------------------------------------------------------
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f $ ma

instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure
    (<*>) (MaybeT mf) (MaybeT ma) = MaybeT $ (liftA2 (<*>)) mf ma

instance Monad m => Monad (MaybeT m) where
    return = pure
    (>>=) (MaybeT ma) f = MaybeT $ do
        maybeA <- ma
        case maybeA of
          Just a -> runMaybeT . f $ a
          Nothing -> return Nothing

instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO
--------------------------------------------------------------------------------
newtype ReaderT r m a = ReaderT { runReaderT :: r -> (m a) }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ fmap f . rma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ (\_ -> pure a)
  (<*>) (ReaderT rmf) (ReaderT rma) = ReaderT $ \r -> (rmf r) <*> (rma r)

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) (ReaderT rma) f = ReaderT (\r -> (rma r) >>= (\a -> runReaderT (f a) r))

instance MonadTrans (ReaderT r) where
    lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

--------------------------------------------------------------------------------
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> ma >>= (\a -> return (a, s))

instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT (\s -> fmap (\(a, s') -> (f a, s')) (g s))

instance Monad m => Applicative (StateT s m) where
  pure a = StateT (\s -> pure (a, s))
  (<*>) (StateT f) (StateT g) =
    StateT
      (\s -> do
         (val, s') <- g s
         (fun, s'') <- f s'
         return (fun val, s''))

instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) (StateT g) f =
    StateT
      (\s -> do
         (a, s') <- g s
         (b, s'') <- runStateT (f a) s'
         return (b, s''))

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO
