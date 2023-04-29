{-# LANGUAGE TypeOperators #-}

module HW0.T1 (
  type(<->)(..)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
) where

import Data.Function

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left       x) = (Left x, Left x)
distrib (Right (x, y)) = (Right x, Right y)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso from to
  where
    from tuple = ((fst tuple, tuple & fst . snd), tuple & snd . snd)
    to tuple = (tuple & fst . fst, (tuple & snd . fst, snd tuple))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso from to
  where
    from (Left          x) = Left (Left x)
    from (Right  (Left x)) = Left (Right x)
    from (Right (Right x)) = Right x

    to (Right        x) = Right (Right x)
    to (Left  (Left x)) = Left x
    to (Left (Right x)) = Right (Left x)
