{-# LANGUAGE LambdaCase #-}

module HW1.T3
  ( Tree(..)
  , tkey
  , tright
  , tleft
  , tdepth
  , tFromList
  , tinsert
  , tmember
  , tsize
  ) where

import Data.Function ((&))

data Meta = Meta
  { mDepth :: Int
  , mSize  :: Int
  }

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)

tkey :: Tree a -> a
tkey Leaf               = undefined
tkey (Branch _ _ key _) = key

tleft :: Tree a -> Tree a
tleft Leaf                = undefined
tleft (Branch _ left _ _) = left

tright :: Tree a -> Tree a
tright Leaf                 = undefined
tright (Branch _ _ _ right) = right

tmeta :: Tree a -> Meta
tmeta Leaf                = undefined
tmeta (Branch meta _ _ _) = meta

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize = \case
  Leaf   -> 0
  branch -> mSize $ tmeta branch

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth = \case
  Leaf   -> 0
  branch -> mDepth $ tmeta branch

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember element tree
  | element == key = True
  | element < key  = tmember element $ tleft tree
  | otherwise      = tmember element $ tright tree
    where
      key = tkey tree

updateMeta :: Tree a -> Tree a -> Meta
updateMeta left right = (Meta newDepth newSize)
  where
    newDepth = mapAndApply tdepth max
    newSize = mapAndApply tsize (+)
    mapAndApply f g = (+) 1 $ g (f left) $ f right

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left element right = (Branch (updateMeta left right) left element right)

rotateImpl :: Meta -> Tree a -> Meta
rotateImpl meta tree = (Meta (newDepth + 1) (newSize + 1))
  where
    newSize = (+) (tsize tree) $ mSize meta
    newDepth = max (tdepth tree) $ mDepth meta

leftRotate :: Tree a -> Tree a
leftRotate tree = (Branch meta newLeft key rightRight)
  where
    rightRight = tree & tright . tright
    key = tree & tkey . tright
    meta = rotateImpl (tmeta newLeft) rightRight
    element = tkey tree
    newLeft = mkBranch (tleft tree) element (tree & tleft . tright)

rightRotate :: Tree a -> Tree a
rightRotate tree = (Branch meta leftLeft key newRight)
  where
    leftLeft = (tree & tleft . tleft)
    key = tree & tkey . tleft
    meta = rotateImpl (tmeta newRight) leftLeft
    element = tkey tree
    newRight = mkBranch (tree & tright . tleft) element (tright tree)

leftRightRotate :: Tree a -> Tree a
leftRightRotate tree = rightRotate (Branch meta (leftRotate left) element right)
  where
    element = tkey tree
    left = tleft tree
    right = tright tree
    meta = tmeta tree

rightLeftRotate :: Tree a -> Tree a
rightLeftRotate tree = leftRotate (Branch meta left element (rightRotate right))
  where
    element = tkey tree
    left = tleft tree
    right = tright tree
    meta = tmeta tree

rotateByRightkey :: Ord a => a -> Tree a -> Tree a
rotateByRightkey element tree
  | element > key = leftRotate tree
  | element < key = rightLeftRotate tree
  | otherwise     = tree
  where
    key = tree & tkey . tright

rotateByLeftkey :: Ord a => a -> Tree a -> Tree a
rotateByLeftkey element tree
  | element < key = rightRotate tree
  | element > key = leftRightRotate tree
  | otherwise     = tree
  where
    key = tree & tkey . tleft

balanceTree :: Ord a => a -> Tree a -> Tree a
balanceTree element tree
  | balance < -1 = rotateByRightkey element tree
  | balance > 1  = rotateByLeftkey element tree
  | otherwise    = tree
  where
    balance = (-) <$> (tdepth . tleft) <*> (tdepth . tright) $ tree

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert element Leaf = mkBranch Leaf element Leaf
tinsert element tree
  | element == key = tree
  | element < key  = balanceTree element (mkBranch (tinsert element left) key right)
  | otherwise      = balanceTree element (mkBranch left key (tinsert element right))
    where
      key = tkey tree
      left = tleft tree
      right = tright tree

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList array = foldr tinsert Leaf array
