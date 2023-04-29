{-# LANGUAGE LambdaCase #-}

module HW1.T6
  ( mcat
  , epart
  ) where

import Data.Foldable (foldMap)

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap unwrap
  where
    unwrap = \case
      Nothing  -> mempty
      (Just a) -> a

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap unwrap
  where
    unwrap = \case
      Left l  -> (l, mempty)
      Right r -> (mempty, r)
