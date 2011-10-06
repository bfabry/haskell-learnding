data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> (Tree a) -> (Tree a)
treeInsert new_el EmptyTree = singleton new_el
treeInsert new_el (Node current_el left_tree right_tree) =
  | new_el == current_el = Node new_el left_tree right_tree
  | new_el < current_el = Node current_el (treeInsert new_el left_tree) right_tree
  | new_el > current_el = Node current_el left_tree (treeInsert new_el right_tree) 

treeHasEl :: (Ord a) => a -> (Tree a) -> Bool
treeHasEl _ EmptyTree = False
treeHasEl el (Node current left right) =
  | el == current = True
  | el < current = treeHasEl el left
  | el > current = treeHasEl el right

instance Functor Tree where
  fmap f (EmptyTree) = EmptyTree
  fmap f (Node el l_tree r_tree) = Node (f el) (fmap f l_tree) (fmap f r_tree)
