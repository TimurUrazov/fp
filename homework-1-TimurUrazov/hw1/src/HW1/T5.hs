module HW1.T5
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty (NonEmpty(..), cons)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn by array =
  let (element, remaining) = span (/= by) array
  in case remaining of
    []   -> element :| []
    _:xs -> cons element $ splitOn by xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith joint (x:|xs) = (++) x $ prepend xs
  where
    prepend []      = []
    prepend (h:t) = (:) joint $ (++) h $ prepend t
