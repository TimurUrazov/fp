{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HW3.Parser
  ( parse
  ) where

import Control.Monad.Combinators.Expr
import Data.ByteString (ByteString, pack)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Void
import GHC.Word
import Numeric
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import HW3.Base

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pNumber :: Parser Rational
pNumber = toRational <$> L.signed sc (lexeme L.scientific)

pFunction :: Parser HiFun
pFunction = skipWhiteSpace $ choice
  [ HiFunNotGreaterThan <$ string "not-greater-than",
    HiFunNotLessThan    <$ string "not-less-than",
    HiFunNotEquals      <$ string "not-equals",
    HiFunNot            <$ string "not",
    HiFunAnd            <$ string "and",
    HiFunOr             <$ string "or",
    HiFunLessThan       <$ string "less-than",
    HiFunGreaterThan    <$ string "greater-than",
    HiFunEquals         <$ string "equals",
    HiFunIf             <$ string "if",
    HiFunDiv            <$ string "div",
    HiFunMul            <$ string "mul",
    HiFunAdd            <$ string "add",
    HiFunSub            <$ string "sub",
    HiFunLength         <$ string "length",
    HiFunToUpper        <$ string "to-upper",
    HiFunToLower        <$ string "to-lower",
    HiFunReverse        <$ string "reverse",
    HiFunTrim           <$ string "trim",
    HiFunList           <$ string "list",
    HiFunRange          <$ string "range",
    HiFunFold           <$ string "fold",
    HiFunPackBytes      <$ string "pack-bytes",
    HiFunUnpackBytes    <$ string "unpack-bytes",
    HiFunEncodeUtf8     <$ string "encode-utf8",
    HiFunDecodeUtf8     <$ string "decode-utf8",
    HiFunZip            <$ string "zip",
    HiFunUnzip          <$ string "unzip",
    HiFunSerialise      <$ string "serialise",
    HiFunDeserialise    <$ string "deserialise",
    HiFunRead           <$ string "read",
    HiFunWrite          <$ string "write",
    HiFunMkDir          <$ string "mkdir",
    HiFunChDir          <$ string "cd",
    HiFunParseTime      <$ string "parse-time",
    HiFunRand           <$ string "rand",
    HiFunEcho           <$ string "echo",
    HiFunCount          <$ string "count",
    HiFunKeys           <$ string "keys",
    HiFunValues         <$ string "values",
    HiFunInvert         <$ string "invert"
  ]

pString :: Parser T.Text
pString = T.pack <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

parseTwoDigitHex :: Parser Word8
parseTwoDigitHex = do
  digits <- count 2 hexDigitChar
  case readHex @Integer digits of
    [(n, "")] -> return $ fromIntegral n
    _ -> fail "invalid hex"

pByteArray :: Parser ByteString
pByteArray = do
  hex <- (between (symbol "[#") (symbol "#]") $ sepEndBy parseTwoDigitHex $ some space1)
  return $ pack hex

pIdentifier :: Parser T.Text
pIdentifier = T.pack <$> (intercalate "-" <$> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy` char '-')

pList :: Parser HiExpr
pList = do
  values <- between (symbol "[") (symbol "]") $ pExp `sepBy` symbol ","
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) values

pValue :: Parser HiValue
pValue = choice
  [ HiValueString <$> pString,
    HiValueNumber <$> pNumber,
    HiValueFunction <$> pFunction,
    HiValueBool <$> pBool,
    HiValueNull <$ string "null",
    HiValueBytes <$> pByteArray,
    HiValueAction <$> pAction
  ]

pBool :: Parser Bool
pBool = skipWhiteSpace $ choice
  [ True <$ string "true",
    False <$ string "false"
  ]

pAction :: Parser HiAction
pAction = skipWhiteSpace $ choice
  [ HiActionCwd <$ string "cwd",
    HiActionNow <$ string "now"
  ]

pDict :: Parser HiExpr
pDict = do
  values <- between (symbol "{") (symbol "}") $ ((,) <$> pExp <*> (symbol ":" *> pExp)) `sepBy` symbol ","
  return $ HiExprDict values

skipWhiteSpace :: Parser a -> Parser a
skipWhiteSpace parser = sc *> parser <* sc

pExpr :: Parser HiExpr
pExpr = do
  value <- skipWhiteSpace $ choice [
      HiExprValue <$> pValue,
      pList,
      pDict,
      parens pExp
    ]
  pE' value

pE' :: HiExpr -> Parser HiExpr
pE' func = (try $ do
  args <- parens $ pExp `sepBy1` symbol ","
  pE' $ HiExprApply func args) <|> (symbol "!" *> (pE' $ HiExprRun func)) <|> (symbol "." *> do
    identifier <- pIdentifier
    pE' $ HiExprApply func $ HiExprValue (HiValueString identifier) : []) <|> return func

pExp :: Parser HiExpr
pExp = makeExprParser pExpr operatorTable

binary
  :: HiFun
  -> (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr)
  -> Parser String
  -> Operator Parser HiExpr
binary func ctor parser = ctor (f <$ parser)
  where
    f x y = HiExprApply (HiExprValue $ HiValueFunction $ func) [x, y]

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binary HiFunMul InfixL $ symbol "*"
    , binary HiFunDiv InfixL $ try $ lexeme (string "/" <* notFollowedBy (string "="))
    ]
  , [ binary HiFunAdd InfixL $ symbol "+"
    , binary HiFunSub InfixL $ symbol "-"
    ]
  , [ binary HiFunNotLessThan InfixN $ symbol ">="
    , binary HiFunLessThan InfixN $ try (symbol "<" <* notFollowedBy (symbol "="))
    , binary HiFunNotGreaterThan InfixN $ symbol "<="
    , binary HiFunGreaterThan InfixN $ try (symbol ">" <* notFollowedBy (symbol "="))
    , binary HiFunEquals InfixN $ symbol "=="
    , binary HiFunNotEquals InfixN $ symbol "/="
    ]
  , [ binary HiFunAnd InfixR $ symbol "&&" ]
  , [ binary HiFunOr InfixR $ symbol "||" ]
  ]

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse s = runParser (pExp <* eof) "" s
