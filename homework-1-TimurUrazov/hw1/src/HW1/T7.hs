module HW1.T7
  ( DotString(..)
  , Fun(..)
  , Inclusive(..)
  , ListPlus(..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last x) scd  = x :+ scd
  (<>) (x:+xs) scd = x :+ (xs <> scd)

data Inclusive a b = This a | That b | Both a b
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This a) (This         b) = (This (a <> b))
  (<>) (That a) (That         b) = (That (a <> b))
  (<>) (Both a b) (Both c     d) = (Both (a <> c) (b <> d))
  (<>) (This a) (That         b) = (Both a b)
  (<>) lhs@(That _) rhs@(This _) = rhs <> lhs
  (<>) (Both a b) (This       c) = (Both (a <> c) b)
  (<>) (This a) (Both b       c) = (Both (a <> b) c)
  (<>) (Both a b) (That       c) = (Both a (b <> c))
  (<>) (That a) (Both b       c) = (Both b (a <> c))

newtype DotString = DS String

instance Semigroup DotString where
  (<>) (DS []) b     = b
  (<>) a (DS [])     = a
  (<>) (DS a) (DS b) = (DS (a ++ "." ++ b))

instance Monoid DotString where
  mempty = DS []

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F f) (F g) = F (f . g)

instance Monoid (Fun a) where
  mempty = (F id)
