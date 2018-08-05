module MaybeLib where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee x f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
-- fromMaybe x Nothing = x
-- fromMaybe _ (Just y) = y
fromMaybe x = mayybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\mx acc -> case mx of
                                Just x -> x : acc
                                otherwise -> acc) []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr helper (Just [])
    where helper Nothing _ = Nothing
          helper _ Nothing = Nothing
          helper (Just x) (Just acc) = Just $ x : acc
