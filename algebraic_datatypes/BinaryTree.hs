--BinaryTree.hs

module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show, Ord)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder (Node Leaf a Leaf) =
  a : []
preorder (Node Leaf a right) =
  (a : []) ++ (preorder right)
preorder (Node left a Leaf) =
  (a : []) ++ (preorder left)
preorder (Node left a right) =
  (a : []) ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder (Node Leaf a Leaf) =
  a : []
inorder (Node Leaf a right) =
  (a : []) ++ (inorder right)
inorder (Node left a Leaf) =
  (a : []) ++ (inorder left)
inorder (Node left a right) =
  (inorder left) ++ (a : [])  ++ (inorder right)

