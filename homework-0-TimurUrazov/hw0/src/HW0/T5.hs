module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import GHC.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz x y = y

ns :: Nat a -> Nat a
ns n f x = n f (f x)

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus a b = (.) <$> a <*> b
nmult a b = (.) <$> (nplus b) <*> (nplus a) $ nz

nFromNatural :: Natural -> Nat a
nFromNatural = fromNatural nz
  where
    fromNatural to from = case from of
      0 -> to
      _ -> fromNatural (ns to) (from - 1)

nToNum :: Num a => Nat a -> a
nToNum n = nplus nz n (+1) 0
