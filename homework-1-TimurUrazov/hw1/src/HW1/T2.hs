{-# LANGUAGE LambdaCase #-}

module HW1.T2
  ( N(..)
  , ncmp
  , ndiv
  , nEven
  , nFromNatural
  , nmod
  , nmult
  , nOdd
  , nplus
  , nsub
  , nToNum
  ) where

import Data.Function
import GHC.Natural (Natural)

data N = Z | S N

dec :: N -> N
dec     Z = undefined
dec (S n) = n

nplus :: N -> N -> N        -- addition
nplus a b = case b of
  Z -> a
  _ -> S $ nplus a $ dec b

nmult :: N -> N -> N        -- multiplication
nmult a b = case b of
  Z -> Z
  _ -> nplus a $ nmult a $ dec b

nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)
nsub a Z = Just a
nsub Z _ = Nothing
nsub a b = nsub (dec a) $ dec b

ncmp :: N -> N -> Ordering  -- comparison      (Do not derive Ord)
ncmp a b = case nsub a b of
  Nothing -> LT
  Just Z  -> EQ
  _       -> GT

nFromNatural :: Natural -> N
nFromNatural = fromNatural Z
  where
    fromNatural to from = case from of
      0 -> to
      _ -> fromNatural (S to) (from - 1)

nToNum :: Num a => N -> a
nToNum = \case
  Z -> 0
  n -> n & (+ 1) . nToNum . dec

data Parity = Odd | Even
  deriving(Eq)

changeParity :: Parity -> Parity
changeParity = \case
  Odd -> Even
  _   -> Odd

getParity :: N -> Parity
getParity = \case
  Z     -> Odd
  (S n) -> changeParity (getParity n)

nEven, nOdd :: N -> Bool
nEven n = getParity n == Odd

nOdd = not . nEven

ndiv :: N -> N -> N
ndiv _ Z = error "Division by zero"
ndiv a b = ndivImpl b Z
  where
    ndivImpl d q = case ncmp a d of
      LT -> q
      _  -> ndivImpl (nplus d b) (S q)

nmod :: N -> N -> N
nmod a b = unwrap remainder
  where
    remainder = nsub a $ nmult b $ ndiv a b
    unwrap Nothing        = undefined
    unwrap (Just content) = content
