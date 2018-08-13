import           Control.Applicative (liftA2)

newtype EitherT a m b = EitherT
  { runEitherT :: m (Either a b)
  }

instance Functor m => Functor (EitherT a m) where
  fmap f (EitherT me) = EitherT $ (fmap . fmap) f me

instance Applicative m => Applicative (EitherT a m) where
  pure = EitherT . pure . pure
  (<*>) (EitherT mf) (EitherT me) = EitherT $ (liftA2 . liftA2) ($) mf me

instance Monad m => Monad (EitherT a m) where
  return = pure
  (>>=) (EitherT me) f =
    EitherT $ do
      eab <- me
      case eab of
        (Left a)  -> return $ Left a
        (Right b) -> runEitherT . f $ b
    -- EitherT $
    -- me >>=
    -- (\eitherAB ->
    --    case eitherAB of
    --      (Left a)  -> return $ Left a
    --      (Right b) -> runEitherT . f $ b)

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right b) = Left b

swapEitherT :: (Functor m) => EitherT a m b -> EitherT b m a
swapEitherT = EitherT . fmap swapEither . runEitherT

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT me) = do
    eab <- me
    case eab of
      (Left a) -> f a
      (Right b) -> g b
