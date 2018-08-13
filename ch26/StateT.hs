newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

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
