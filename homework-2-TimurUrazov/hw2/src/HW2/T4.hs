{-# LANGUAGE LambdaCase #-}

module HW2.T4
  ( Expr(..)
  , Prim(..)
  , State(..)
  , eval
  , evalAbstract
  , evalBinary
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where

import Control.Monad

import HW2.T1

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f oldState = S $ \s ->
  mapAnnotated f $ runS oldState s

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState oldState = S $ \s ->
  let (newState :# s1) = runS oldState s
  in runS newState s1

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  (-) x y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  (/) x y = Op (Div x y)
  fromRational x = Val (fromRational x)

evalUnary
  :: Monad m
  => Expr
  -> (Double -> Prim Double)
  -> (Double -> Double)
  -> (([Prim Double] -> [Prim Double]) -> m ())
  -> m Double
evalUnary e ctor f modif = do
  res <- evalAbstract modif e
  modif ((ctor res):)
  pure $ f res

evalBinary
  :: Monad m
  => Expr
  -> Expr
  -> (Double -> Double -> Prim Double)
  -> (Double -> Double -> m Double)
  -> (([Prim Double] -> [Prim Double]) -> m ())
  -> m Double
evalBinary e1 e2 ctor process modif = do
  res1 <- evalAbstract modif e1
  res2 <- evalAbstract modif e2
  modif ((ctor res1 res2):)
  process res1 res2

evalAbstract
  :: Monad m
  => (([Prim Double] -> [Prim Double]) -> m ())
  -> Expr
  -> m Double
evalAbstract _ (Val     v) = pure v
evalAbstract modif (Op op) = lookupOp modif
  where
    lookupOp = case op of
      (Abs     y) -> evalUnary y Abs abs
      (Sgn     y) -> evalUnary y Sgn signum
      (Add y1 y2) -> evalBinary y1 y2 Add $ \x y -> pure $ x + y
      (Sub y1 y2) -> evalBinary y1 y2 Sub $ \x y -> pure $ x - y
      (Mul y1 y2) -> evalBinary y1 y2 Mul $ \x y -> pure $ x * y
      (Div y1 y2) -> evalBinary y1 y2 Div $ \x y -> pure $ x / y

eval :: Expr -> State [Prim Double] Double
eval = evalAbstract modifyState
