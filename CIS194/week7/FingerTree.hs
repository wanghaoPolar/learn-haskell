module FingerTree where

data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a)
type Size = Int

toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y

tag :: Tree v a -> v
tag (Leaf v _)     = v
tag (Branch v _ _) = v

treeHead :: Tree v a -> a
treeHead (Leaf _ a)     = a
treeHead (Branch _ x _) = treeHead x

leaf :: a -> Tree Size a
leaf = Leaf 1

branch :: Tree Size a -> Tree Size a -> Tree Size a
branch x y = Branch (tag x + tag y) x y

(!!!) :: Tree Size a -> Int -> a
(Leaf _ a)      !!! 0 = a
(Branch _ x y)  !!! n
     | n < tag x     = x !!! n
     | otherwise     = y !!! (n - tag x)

type Priority = Int
