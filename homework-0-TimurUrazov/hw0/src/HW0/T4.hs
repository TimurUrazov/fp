module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import GHC.Natural

repeat' :: a -> [a]             -- behaves like Data.List.repeat
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]  -- behaves like Data.List.map
map' func = fix (\f x -> case x of
  []     -> []
  (h:t)  -> func h : f t)

fib :: Natural -> Natural       -- computes the n-th Fibonacci number
fib = fix (\f a b n -> case n of
  0   -> a
  num -> f b (a + b) (num - 1)) 0 1

fac :: Natural -> Natural
fac s = fix (\f a n -> case n of
  0   -> 1
  1   -> a
  num -> f (a * (num - 1)) (num - 1)) s s
