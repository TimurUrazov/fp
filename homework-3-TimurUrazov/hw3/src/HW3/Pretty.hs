{-# LANGUAGE TypeApplications #-}

module HW3.Pretty
  ( prettyValue
  ) where

import Data.Foldable (toList)
import Data.ByteString hiding (map)
import GHC.Real
import qualified Data.Map as Map (toAscList)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import qualified Data.Text as T
import Numeric
import Prettyprinter
import Prettyprinter.Render.Terminal

import HW3.Base

processFraction :: Integral a => a -> (a, a) -> Bool -> Doc AnsiStyle
processFraction denum (0, frac) _ = hcat [pretty (toInteger frac), slash, pretty (toInteger denum)]
processFraction denum (int, frac) sign = case sign of
  True  -> format "+" denum (0, frac)
  False -> format "-" (abs denum) (0, abs frac)
  where
    format str x y = hsep [pretty (toInteger int), pretty str, processFraction x y sign]

toHex :: ByteString -> Doc AnsiStyle
toHex = hsep . map (pretty . (\x -> if Prelude.length x == 1 then "0" ++ x else x) . flip showHex "") . unpack

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue hiValue = case hiValue of
  HiValueNumber number   -> case fromRationalRepetendUnlimited number of
    (num, Nothing) -> case number of
      (numer :% 1)     -> pretty numer
      _                -> pretty $ formatScientific Fixed Nothing num
    _              -> case number of
      (numer :% denumerator) -> processFraction denumerator (quotRem numer denumerator) $ (signum numer) * (signum denumerator) == 1
  HiValueBool number     -> case number of
    True  -> pretty "true"
    False -> pretty "false"
  HiValueFunction number -> case number of
    HiFunDiv            -> pretty "div"
    HiFunMul            -> pretty "mul"
    HiFunAdd            -> pretty "add"
    HiFunSub            -> pretty "sub"
    HiFunNot            -> pretty "not"
    HiFunAnd            -> pretty "and"
    HiFunOr             -> pretty "or"
    HiFunLessThan       -> pretty "less-than"
    HiFunGreaterThan    -> pretty "greater-than"
    HiFunEquals         -> pretty "equals"
    HiFunNotLessThan    -> pretty "not-less-than"
    HiFunNotGreaterThan -> pretty "not-greater-than"
    HiFunNotEquals      -> pretty "not-equals"
    HiFunIf             -> pretty "if"
    HiFunLength         -> pretty "length"
    HiFunToUpper        -> pretty "to-upper"
    HiFunToLower        -> pretty "to-lower"
    HiFunReverse        -> pretty "reverse"
    HiFunTrim           -> pretty "trim"
    HiFunList           -> pretty "list"
    HiFunRange          -> pretty "range"
    HiFunFold           -> pretty "fold"
    HiFunPackBytes      -> pretty "pack-bytes"
    HiFunUnpackBytes    -> pretty "unpack-bytes"
    HiFunEncodeUtf8     -> pretty "encode-utf8"
    HiFunDecodeUtf8     -> pretty "decode-utf8"
    HiFunZip            -> pretty "zip"
    HiFunUnzip          -> pretty "unzip"
    HiFunSerialise      -> pretty "serialise"
    HiFunDeserialise    -> pretty "deserialise"
    HiFunRead           -> pretty "read"
    HiFunWrite          -> pretty "write"
    HiFunMkDir          -> pretty "mkdir"
    HiFunChDir          -> pretty "cd"
    HiFunParseTime      -> pretty "parse-time"
    HiFunRand           -> pretty "rand"
    HiFunEcho           -> pretty "echo"
    HiFunCount          -> pretty "count"
    HiFunKeys           -> pretty "keys"
    HiFunValues         -> pretty "values"
    HiFunInvert         -> pretty "invert"
  HiValueNull            -> pretty "null"
  HiValueString s        -> hcat [dquote, pretty s, dquote]
  HiValueList l          -> case toList l of
    [] -> pretty "[ ]"
    _  -> hsep [lbracket, hsep (punctuate comma (map prettyValue (toList l))), rbracket]
  HiValueBytes bytes     -> case unpack bytes of
    [] -> pretty "[# #]"
    _  -> hsep [lbracket <> pretty "#", toHex bytes, pretty "#" <> rbracket]
  HiValueAction action   -> case action of
    HiActionRead path    -> hcat [pretty "read", parens (prettyValue (HiValueString $ T.pack path))]
    HiActionWrite path s -> hcat [pretty "write", parens (prettyValue (HiValueString $ T.pack path) <> comma <> space <> prettyValue (HiValueBytes s))]
    HiActionMkDir path   -> hcat [pretty "mkdir", parens (prettyValue (HiValueString $ T.pack path))]
    HiActionChDir path   -> hcat [pretty "cd", parens (prettyValue (HiValueString $ T.pack path))]
    HiActionCwd          -> pretty "cwd"
    HiActionNow          -> pretty "now"
    HiActionRand n1 n2   -> hcat [pretty "rand", encloseSep lparen rparen (comma <> space) [pretty n1, pretty n2]]
    HiActionEcho s       -> hcat [pretty "echo", parens (prettyValue (HiValueString s))]
  HiValueTime time       -> pretty "parse-time" <> (parens $ enclose dquote dquote $ viaShow time)
  HiValueDict m          -> hsep [lbrace, hsep (punctuate comma (fmap (\(k, v) -> hcat [prettyValue k, colon, space <> prettyValue v]) (Map.toAscList m))), rbrace]
