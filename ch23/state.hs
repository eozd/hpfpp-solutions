module State where

newtype State' s a = State'
  { runState' :: s -> (a, s)
  }

instance Functor (State' s) where
  fmap f (State' fs) =
    State' $ \s ->
      let (a', s') = fs s
       in (f a', s')

instance Applicative (State' s) where
  pure a = State' $ \s -> (a, s)
  (<*>) (State' f) (State' g) =
    State' $ \s ->
      let (a, state1) = g s
          (newFunc, state2) = f state1
       in (newFunc a, state2)

instance Monad (State' s) where
  return = pure
  (>>=) (State' g) f =
    State' $ \s ->
      let (a, state1) = g s
          (State' h) = f a
       in h state1

get :: State' s s
get = State' $ \s -> (s, s)

put :: s -> State' s ()
put newState = State' $ \_ -> ((), newState)

exec :: State' s a -> s -> s
exec (State' sa) s = snd $ sa s

eval :: State' s a -> s -> a
eval (State' sa) s = fst $ sa s

modify :: (s -> s) -> State' s ()
modify f = State' $ \s -> ((), f s)
