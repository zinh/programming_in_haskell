data List a = Empty | Cons a (List a) deriving (Show,Eq, Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq)

singleton :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert a EmptyTree = singleton a
treeInsert a t@(Node root left right)
  | a == root = t
  | a < root = Node root (treeInsert a left) right
  | a > root = Node root left (treeInsert a right)

buildTree :: (Ord a) => [a] -> Tree a
buildTree = foldl (\memo x -> treeInsert x memo) EmptyTree

exist :: (Ord a) => a -> Tree a -> Bool
exist a EmptyTree = False
exist a (Node root left right)
  | a == root = True
  | a < root = exist a left
  | otherwise = exist a right

height :: Tree a -> Int
height EmptyTree = 0
height (Node _ left right) = 1 + (max (height left) (height right))
