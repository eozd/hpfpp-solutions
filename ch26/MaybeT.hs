import           Control.Applicative (liftA2)

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (<*>) (MaybeT mf) (MaybeT ma) = MaybeT $ (liftA2 . liftA2) ($) mf ma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) (MaybeT ma) f =
    MaybeT $
    ma >>=
    (\maybeA ->
       case maybeA of
         Just a  -> runMaybeT . f $ a
         Nothing -> return Nothing)
