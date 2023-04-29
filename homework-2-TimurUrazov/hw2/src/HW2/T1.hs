{-# LANGUAGE LambdaCase #-}

module HW2.T1
  ( Annotated(..)
  , Except(..)
  , Fun(..)
  , Quad(..)
  , List(..)
  , Option(..)
  , Pair(..)
  , Prioritised(..)
  , Stream(..)
  , Tree(..)
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapQuad
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapStream
  , mapTree
  ) where

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e
infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a
infixr 5 :>

data List a = Nil | a :. List a
infixr 5 :.

data Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None     = None
mapOption f (Some a) = Some (f a)

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a b) = P (f a) (f b)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = (f a) :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f = \case
  Error   e -> Error e
  Success a -> Success (f a)

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f = \case
  Low    a -> Low (f a)
  Medium a -> Medium (f a)
  High   a -> High (f a)

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (a :> b) = (f a) :> mapStream f b

mapList :: (a -> b) -> (List a -> List b)
mapList f = \case
  Nil      -> Nil
  (a :. b) -> (f a) :. mapList f b

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F x) = F (\i -> case (x i) of a -> f a)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f = \case
  Leaf         -> Leaf
  Branch a b c -> Branch (mapTree f a) (f b) (mapTree f c)
