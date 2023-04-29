{-# LANGUAGE LambdaCase #-}

module HW2.T5
  ( EvaluationError (..)
  , ExceptState(..)
  , eval
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , throwExceptState
  , wrapExceptState
  ) where

import Control.Monad

import HW2.T1
import HW2.T4 (Expr(..) , Prim(..), evalAbstract, evalBinary)

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f oldState = ES $ \s ->
  mapExcept (mapAnnotated f) $ runES oldState s

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState oldState = ES $ \s ->
  case runES oldState s of
    Error e          -> Error e
    Success (a :# b) -> runES a b

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Op (Div y1 y2)) = evalBinary y1 y2 Div process modifyExceptState
  where
    process = \x y -> do
      when (y == 0) $ throwExceptState DivideByZero
      pure $ x / y

eval x = evalAbstract modifyExceptState x
