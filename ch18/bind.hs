import Control.Monad (join)

join' :: Maybe (Maybe a) -> Maybe a
join' Nothing = Nothing
join' (Just Nothing) = Nothing
join' (Just (Just x)) = Just x

bind :: Monad m => m a -> (a -> m b) -> m b
bind mx f = join $ fmap f mx
