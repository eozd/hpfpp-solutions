import Control.Applicative (liftA2)

newtype Compose f g a = Compose
  { runCompose :: f (g a)
  }

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = Compose . pure . pure
    
    (<*>) (Compose fgf) (Compose fga) = Compose $ (liftA2 . liftA2) ($) fgf fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga
