{-# LANGUAGE RankNTypes #-}

-- type Nat f g = forall a. f a -> g a

maybeToList :: forall a. Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
