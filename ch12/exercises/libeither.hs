module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' = foldr helper []
  where helper (Left a) acc = a : acc
        helper (Right _) acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr helper []
  where helper (Right a) acc = a : acc
        helper (Left _) acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr helper ([], [])
  where helper (Left a) (accLeft, accRight) = (a : accLeft, accRight)
        helper (Right a) (accLeft, accRight) = (accLeft, a : accRight)

-- eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
-- eitherMaybe' _ (Left _) = Nothing
-- eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f = either' (\_ -> Nothing) (Just . f)
