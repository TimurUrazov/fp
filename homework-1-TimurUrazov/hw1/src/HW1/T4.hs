module HW1.T4
  ( tfoldr
  , treeToList
  ) where

import HW1.T3 (Tree(..), tkey, tleft, tright)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ resid Leaf    = resid
tfoldr func resid tree = tfoldr func (func key $ tfoldr func resid right) left
  where
    left = tleft tree
    right = tright tree
    key = tkey tree

treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []
