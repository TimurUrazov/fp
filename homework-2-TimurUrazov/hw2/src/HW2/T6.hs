{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module HW2.T6
  ( ParseError(..)
  , Parser(..)
  , parseError
  , parseExpr
  , pChar
  , pEof
  , runP
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Scientific
import GHC.Natural

import HW2.T1 hiding (P)
import HW2.T4
import HW2.T5

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P p) str = case runES p (0, str) of
    Error e          -> Error e
    Success (a :# _) -> Success a

-- | When we have empty string in our state, then we got error
-- at the position, which is defined in state. If the string is
-- not empty, then we change state, incrementing the position
-- and shortening the rest of the string by one character from
-- the beggining.
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error $ ErrorAtPos pos

instance Alternative Parser where
  empty = parseError
  (P p1) <|> (P p2) = P $ ES $ \s -> case (runES p1 s) of
    Error _ -> runES p2 s
    x       -> x

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    []     -> Success (() :# (pos, s))
    _      -> Error (ErrorAtPos pos)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = do
  ch <- pChar
  when (not $ predicate ch) $ parseError
  return ch

parseChar :: Char -> Parser Char
parseChar a = satisfy (== a)

parseInteger :: Parser String
parseInteger = some $ satisfy isDigit

parseBrackets :: Parser Expr
parseBrackets = do
  _ <- skipWhiteSpace
  _ <- parseChar '('
  _ <- skipWhiteSpace
  expr <- parseExpression
  _ <- skipWhiteSpace
  _ <- parseChar ')'
  return expr

digitToInteger :: Char -> Integer
digitToInteger = toInteger . digitToInt

toIntegerLoop :: Integer -> Char -> Integer
toIntegerLoop 0 = digitToInteger
toIntegerLoop y = ((+) $ (*) y 10) . digitToInteger

convertToDouble :: String -> Int -> Double
convertToDouble x y = toRealFloat $ scientific (foldl toIntegerLoop 0 x) y

parseDouble :: Parser Expr
parseDouble = skipWhiteSpace *> do
  before <- parseInteger
  point <- optional $ parseChar '.'
  case point of
    Nothing -> return $ Val $ convertToDouble before 0
    Just _  -> do
      after <- parseInteger
      return $ Val $ convertToDouble (before ++ after) $ negate $ length after

skipWhiteSpace :: Parser String
skipWhiteSpace = many $ satisfy isSpace

parseLow :: Parser Expr
parseLow = parseBrackets <|> parseDouble

parseOperation :: Expr -> Expr -> Char -> Parser Expr
parseOperation x y = \case
  '*' -> convertOp Mul
  '/' -> convertOp Div
  '+' -> convertOp Add
  '-' -> convertOp Sub
  _   -> parseError
  where
    convertOp ctor = return $ Op $ ctor x y

parseLevel :: [Char] -> Expr -> Parser Expr -> Parser Expr
parseLevel operators prev nextLevel = skipWhiteSpace *> do
  operator <- msum $ map parseChar operators
  cur <- nextLevel
  next <- parseOperation prev cur operator
  parseLevel operators next nextLevel <|> return next

parse :: Parser Expr -> [Char] -> Parser Expr
parse prevParser operators = skipWhiteSpace *> do
  operand <- prevParser
  parseLevel operators operand prevParser <|> return operand

parseExpression :: Parser Expr
parseExpression = foldl parse parseLow [['*', '/'], ['-', '+']]

pExpr :: Parser Expr
pExpr = parseExpression <* skipWhiteSpace <* pEof

parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpr
