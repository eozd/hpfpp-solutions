myIterate :: (a -> a) -> a -> [a]
myIterate f initX = initX : myIterate f (f initX)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f initX = case f initX of
    Just (prevX, nextX) -> prevX : myUnfoldr f nextX
    Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq)

instance (Show a) => Show (Tree a) where
    show tree = go tree 0
        where go Leaf _ = ""
              go (Node left x right) depth =
                  go left (depth + 1) ++
                  replicate depth '\t' ++ show x ++ "\n" ++
                  go right (depth + 1)

ourTree = Node (Node (Node Leaf 1 Leaf) 3 (Node Leaf 4 Leaf)) 5 (Node (Node Leaf 7 Leaf) 8 (Node Leaf 10 Leaf))

unfold :: (a -> Maybe (a, b, a)) -> a -> Tree b
unfold f initX = case f initX of
    Just (leftRes, rootRes, rightRes) -> Node (unfold f leftRes)
                                              rootRes
                                              (unfold f rightRes)
    Nothing -> Leaf

treeBuild :: Integer -> Tree Integer
treeBuild n = unfold helper 0
    where helper x = if x == n
                        then Nothing
                        else Just (x + 1, x, x + 1)
