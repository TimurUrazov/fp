{-# LANGUAGE LambdaCase #-}

module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1
import HW2.T2

joinOption :: Option (Option a) -> Option a
joinOption = \case
  None   -> None
  Some x -> mapOption id x

joinExcept :: Except e (Except e a) -> Except e a
joinExcept = \case
  Error e   -> Error e
  Success a -> mapExcept id a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# b) :# c) = a :# c <> b

joinList :: List (List a) -> List a
joinList = fld conc Nil
  where
    fld _ z Nil       = z
    fld f z (x :. xs) = f x (fld f z xs)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F x) = F $ \i ->
  let (F a) = x i
  in a i
