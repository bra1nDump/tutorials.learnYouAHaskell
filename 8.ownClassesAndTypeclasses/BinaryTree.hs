module BinaryTree
  ( BinaryTree(..)
  , insert
  ) where

data BinaryTree a =
  EmptyTree
  | Node a (BinaryTree a) (BinaryTree a)
  deriving (Show)

singleton :: (Ord a) => a -> BinaryTree a
singleton x = Node x EmptyTree EmptyTree

insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert x EmptyTree = singleton x
insert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (insert x left) right
  | x > a  = Node a left (insert x right)
