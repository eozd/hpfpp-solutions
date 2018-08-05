module BinaryTree where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

insert' :: (Ord a) => a -> Tree a -> Tree a
insert' a Leaf = Node Leaf a Leaf
insert' a (Node left x right)
  | a == x = Node left x right
  | a < x = Node (insert' a left) x right
  | a > x = Node left x (insert' a right)

map' :: (a -> b) -> Tree a -> Tree b
map' _ Leaf = Leaf
map' f (Node left x right) = Node (map' f left) (f x) (map' f right)

testTree :: Tree Integer
testTree = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: Tree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay = if map' (+1) testTree == mapExpected
            then print "yup, okay!"
            else error "test failed!"

preorder :: Tree a -> [a]
preorder Leaf = []
preorder (Node left x right) = x : (preorder left ++ preorder right)

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node left x right) = inorder left ++ (x : inorder right)

postorder :: Tree a -> [a]
postorder Leaf = []
postorder (Node left x right) = postorder left ++ postorder right ++ [x]

preorderOkay :: Bool
preorderOkay = preorder testTree == [1, 3, 4]

inorderOkay :: Bool
inorderOkay = inorder testTree == [3, 1, 4]

postorderOkay :: Bool
postorderOkay = postorder testTree == [3, 4, 1]

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left x right) =
    let rightVal = f x (foldTree f z right)
    in foldTree f rightVal left

foldTestTree :: Tree Integer
foldTestTree = Node
                (Node
                    (Node Leaf 1 Leaf)
                    3
                    (Node Leaf 4 Leaf))
                5
                (Node
                    (Node Leaf 6 Leaf)
                    7
                    (Node Leaf 8 Leaf))

foldTrace :: (Show a) => Tree a -> String
foldTrace = foldTree (\x acc -> concat ["(", show x, "+", acc, ")"]) "0"

foldOkay :: Bool
foldOkay = foldTrace foldTestTree == "(1+(3+(4+(5+(6+(7+(8+0)))))))"
