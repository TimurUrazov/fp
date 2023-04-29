{-# LANGUAGE LambdaCase #-}

module HW2.T2
  ( conc
  , distAnnotated
  , distExcept
  , distFun
  , distQuad
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distStream
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapQuad
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapStream
  ) where

import HW2.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption = \case
  (Some x, Some y) -> Some (x, y)
  _                -> None

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a b c d, Q e f g h) = Q (a, e) (b, f) (c, g) (d, h)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# b, c :# d) = (a , c) :# b <> d

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept = \case
  (Success a, Success b) -> Success (a, b)
  (Success _, Error   x) -> Error x
  (Error y,           _) -> Error y

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised = \case
  (High a,   b) -> High (a, unwrap b)
  (a, High   b) -> High (unwrap a, b)
  (Medium a, b) -> Medium (a, unwrap b)
  (a, Medium b) -> Medium (unwrap a, b)
  (a,        b) -> Low (unwrap a, unwrap b)
  where
    unwrap :: Prioritised a -> a
    unwrap = \case
      High   a -> a
      Medium a -> a
      Low    a -> a

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> b, c :> d) = (a, c) :> distStream (b, d)

conc :: List a -> List a -> List a
conc Nil a      = a
conc (a :. b) c = a :. conc b c

distList :: (List a, List b) -> List (a, b)
distList = \case
  (Nil,    _) -> Nil
  (_,    Nil) -> Nil
  (a :. b, l) -> conc (assoc a l) (distList (b, l))
    where
      assoc :: a -> List b -> List (a, b)
      assoc c (d :. e) = (c, d) :. assoc c e
      assoc _ Nil      = Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F a, F b) = F (\i -> (a i, b i))

wrapOption :: a -> Option a
wrapOption = Some

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

wrapList :: a -> List a
wrapList = (:. Nil)

wrapFun :: a -> Fun i a
wrapFun = F . const
