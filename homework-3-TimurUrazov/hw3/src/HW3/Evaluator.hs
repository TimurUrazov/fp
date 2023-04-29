{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase       #-}

module HW3.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib
import Control.Monad.Except
import Codec.Serialise
import qualified Data.ByteString as B
import Data.Foldable (toList)
import Data.Semigroup
import qualified Data.Sequence as Seq (Seq(..), drop, empty, fromList, length, lookup, reverse, take)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime(..), addUTCTime, diffUTCTime)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Map as Map (elems, fromList, fromListWith, keys, lookup, map, toAscList)
import GHC.Real
import Text.Read (readMaybe)

import HW3.Base

evalSingleArgumentFunction
  :: (HiMonad m)
  => [HiExpr]
  -> (HiValue -> ExceptT HiError m HiValue)
  -> ExceptT HiError m HiValue
evalSingleArgumentFunction [arg] process = do
  arg' <- evalExpr arg
  process arg'
evalSingleArgumentFunction _ _ = throwError HiErrorArityMismatch

evalDoubleArgumentFunction
  :: (HiMonad m)
  => [HiExpr]
  -> (HiValue -> HiValue -> ExceptT HiError m HiValue)
  -> ExceptT HiError m HiValue
evalDoubleArgumentFunction [arg1, arg2] process = do
  arg1' <- evalExpr arg1
  arg2' <- evalExpr arg2
  process arg1' arg2'
evalDoubleArgumentFunction _ _ = throwError HiErrorArityMismatch

evalSingleOrDoubleArgumentFunction
  :: (HiMonad m)
  => [HiExpr]
  -> (HiValue -> ExceptT HiError m HiValue)
  -> (HiValue -> HiValue -> ExceptT HiError m HiValue)
  -> ExceptT HiError m HiValue
evalSingleOrDoubleArgumentFunction args processSingle processDouble = do
  case length args of
    1 -> evalSingleArgumentFunction args processSingle
    2 -> evalDoubleArgumentFunction args processDouble
    _ -> throwError HiErrorArityMismatch

evalExpr :: (HiMonad m) => HiExpr -> ExceptT HiError m HiValue
evalExpr (HiExprValue v) = return v
evalExpr (HiExprDict d) = do
  d' <- mapM (\(e1, e2) -> do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    return (v1, v2)) d
  return $ HiValueDict $ Map.fromList d'

evalExpr (HiExprRun expr) = do
  e <- evalExpr expr
  case e of
    (HiValueAction action) -> lift $ runAction action
    _ -> throwError HiErrorInvalidArgument

evalExpr (HiExprApply expr args) = do
  e <- evalExpr expr

  case e of
    HiValueDict d                       ->
      evalSingleArgumentFunction args $ \arg -> do
        case Map.lookup arg d of
          Just v  -> return v
          Nothing -> return $ HiValueNull

    HiValueFunction HiFunKeys           ->
      evalSingleArgumentFunction args $ \arg -> do
        case arg of
          HiValueDict d -> return $ HiValueList $ Seq.fromList $ Map.keys d
          _             -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunValues         ->
      evalSingleArgumentFunction args $ \arg -> do
        case arg of
          HiValueDict d -> return $ HiValueList $ Seq.fromList $ Map.elems d
          _             -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunCount          ->
      evalSingleArgumentFunction args $ \arg -> do
        case arg of
          HiValueList l   -> return $ createDictFromHiValuesList $ map (\x -> (x, 1)) $ toList l
          HiValueString s -> return $ createDictFromHiValuesList $ map (\x -> (HiValueString $ T.singleton x, 1)) $ T.unpack s
          HiValueBytes b  -> return $ createDictFromHiValuesList $ map (\x -> (HiValueNumber $ fromIntegral x, 1)) $ B.unpack b
          _               -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunInvert         ->
      evalSingleArgumentFunction args $ \arg -> do
        case arg of
          HiValueDict d -> return $ HiValueDict $ Map.map (HiValueList . Seq.fromList) $ Map.fromListWith (++) $ map (\(k, v) -> (v, [k])) $ Map.toAscList d
          _             -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunDiv            ->
      evalDoubleArgumentFunction args $ \arg1 arg2 -> do
        case (arg1, arg2) of
          (HiValueNumber n1, HiValueNumber n2) ->
            if n2 == 0 then throwError HiErrorDivideByZero else return $ HiValueNumber $ n1 / n2
          (HiValueString s1, HiValueString s2) ->
            return $ HiValueString $ T.intercalate (T.pack "/") [s1, s2]
          _                                    -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunMul            ->
      evalDoubleArgumentFunction args $ \arg1 arg2 -> do
        case (arg1, arg2) of
          (HiValueNumber n1, HiValueNumber n2) -> return $ HiValueNumber $ n1 * n2
          (HiValueString s, HiValueNumber n)   -> applyStimesToMonoid n s HiValueString
          (HiValueNumber n, HiValueString s)   -> applyStimesToMonoid n s HiValueString
          (HiValueList l, HiValueNumber n)     -> applyStimesToMonoid n l HiValueList
          (HiValueNumber n, HiValueList l)     -> applyStimesToMonoid n l HiValueList
          (HiValueBytes b, HiValueNumber n)    -> applyStimesToMonoid n b HiValueBytes
          (HiValueNumber n, HiValueBytes b)    -> applyStimesToMonoid n b HiValueBytes
          _                                    -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunAdd            ->
      evalDoubleArgumentFunction args $ \arg1 arg2 -> do
        case (arg1, arg2) of
          (HiValueNumber n1, HiValueNumber n2) -> return $ HiValueNumber $ n1 + n2
          (HiValueString s1, HiValueString s2) -> return $ HiValueString $ s1 <> s2
          (HiValueList l1, HiValueList l2)     -> return $ HiValueList $ l1 <> l2
          (HiValueBytes b1, HiValueBytes b2)   -> return $ HiValueBytes $ b1 <> b2
          (HiValueTime t, HiValueNumber n)     -> return $ HiValueTime $ addUTCTime (fromRational n) t
          (HiValueNumber n, HiValueTime t)     -> return $ HiValueTime $ addUTCTime (fromRational n) t
          _                                    -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunSub            ->
      evalDoubleArgumentFunction args $ \arg1 arg2 -> do
        case (arg1, arg2) of
          (HiValueNumber n1, HiValueNumber n2) -> return $ HiValueNumber $ n1 - n2
          (HiValueTime t1, HiValueTime t2)     -> return $ HiValueNumber $ toRational $ diffUTCTime t1 t2
          _                                    -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunNot            ->
      evalSingleArgumentFunction args $ \arg -> do
        case arg of
          (HiValueBool b) -> return $ HiValueBool $ not b
          _               -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunAnd            -> do
      when (length args /= 2) $ throwError HiErrorArityMismatch
      arg1 <- evalExpr (args !! 0)
      case arg1 of
        HiValueBool b1 -> if b1 then evalExpr (args !! 1) else return arg1
        HiValueNull    -> return arg1
        _              -> evalExpr (args !! 1)

    HiValueFunction HiFunOr             -> do
      when (length args /= 2) $ throwError HiErrorArityMismatch
      arg1 <- evalExpr (args !! 0)
      case arg1 of
        HiValueBool b1 -> if b1 then return arg1 else evalExpr (args !! 1)
        HiValueNull    -> evalExpr (args !! 1)
        _              -> return arg1

    HiValueFunction HiFunEquals         ->
      evalDoubleArgumentFunction args $ \arg1 arg2 ->
        HiValueBool <$> equalValues arg1 arg2

    HiValueFunction HiFunNotEquals      ->
      evalDoubleArgumentFunction args $ \arg1 arg2 ->
        HiValueBool . not <$> equalValues arg1 arg2

    HiValueFunction HiFunLessThan       ->
      evalDoubleArgumentFunction args $ \arg1 arg2 -> do
        greater <- greaterValue arg1 arg2
        equal <- equalValues arg1 arg2
        return $ HiValueBool $ not greater && not equal

    HiValueFunction HiFunGreaterThan    ->
      evalDoubleArgumentFunction args $ \arg1 arg2 ->
        HiValueBool <$> greaterValue arg1 arg2

    HiValueFunction HiFunNotGreaterThan ->
      evalDoubleArgumentFunction args $ \arg1 arg2 ->
        HiValueBool . not <$> greaterValue arg1 arg2

    HiValueFunction HiFunNotLessThan    ->
      evalDoubleArgumentFunction args $ \arg1 arg2 -> do
        greater <- greaterValue arg1 arg2
        equal <- equalValues arg1 arg2
        return $ HiValueBool $ greater || equal

    HiValueFunction HiFunIf             -> do
      when(length args /= 3) $ throwError HiErrorArityMismatch
      arg1 <- evalExpr (args !! 0)
      case arg1 of
        HiValueBool b -> if b then evalExpr (args !! 1) else evalExpr (args !! 2)
        _             -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunLength         ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueString s) -> return $ HiValueNumber $ fromIntegral $ T.length s
          (HiValueList l)   -> return $ HiValueNumber $ fromIntegral $ length l
          (HiValueBytes b)  -> return $ HiValueNumber $ fromIntegral $ B.length b
          _                 -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunToUpper        ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueString s) -> return $ HiValueString $ T.toUpper s
          _                 -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunToLower        ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueString s) -> return $ HiValueString $ T.toLower s
          _                 -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunTrim           ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueString s) -> return $ HiValueString $ T.strip s
          _                 -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunReverse        ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueBytes b)  -> return $ HiValueBytes $ B.reverse b
          (HiValueString s) -> return $ HiValueString $ T.reverse s
          (HiValueList l)   -> return $ HiValueList $ Seq.reverse l
          _                 -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunList           -> do
      evaluatedArgs <- mapM evalExpr args
      return $ HiValueList $ Seq.fromList evaluatedArgs

    HiValueFunction HiFunRange          ->
      evalDoubleArgumentFunction args $ \arg1 arg2 -> do
        case (arg1, arg2) of
          (HiValueNumber n1, HiValueNumber n2) -> return $ HiValueList $ Seq.fromList $ map HiValueNumber [n1..n2]
          _                                    -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunFold           ->
      evalDoubleArgumentFunction args $ \arg1 arg2 -> do
        case (arg1, arg2) of
          (HiValueFunction f, HiValueList l) -> case l of
            Seq.Empty      -> return $ HiValueNull
            (x Seq.:<| xs) -> foldM (\acc el -> evalExpr $ HiExprApply (HiExprValue (HiValueFunction f)) (map HiExprValue [acc, el])) x xs
          _                                  -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunPackBytes      ->
      evalSingleArgumentFunction args $ \arg1 ->
        case arg1 of
          HiValueList l -> do
            if any (\x -> case x of
                                HiValueNumber (n :% 1) -> n < 0 || n > 255
                                _                      -> True) $ toList l
              then throwError HiErrorInvalidArgument
              else return $ HiValueBytes $ B.pack $ map (\x -> case x of
                                                                    HiValueNumber (n :% 1) -> fromInteger n
                                                                    _                      -> error "unreachable") $ toList l
          _             -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunUnpackBytes    ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          HiValueBytes b -> return $ HiValueList $ Seq.fromList $ map (\x -> HiValueNumber $ (toInteger x) :% 1) $ B.unpack b
          _              -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunEncodeUtf8     ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          HiValueString s -> return $ HiValueBytes $ TE.encodeUtf8 s
          _               -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunDecodeUtf8     ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          HiValueBytes b -> case TE.decodeUtf8' b of
            Left _  -> return $ HiValueNull
            Right s -> return $ HiValueString s
          _ -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunZip            ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueBytes b) -> return $ HiValueBytes $ toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } $ fromStrict b
          _                -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunUnzip          ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueBytes b) -> return $ HiValueBytes $ toStrict $ decompress $ fromStrict b
          _                -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunSerialise      ->
      evalSingleArgumentFunction args $ \arg1 -> do
        return $ HiValueBytes $ toStrict $ serialise arg1

    HiValueFunction HiFunDeserialise    ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueBytes b) -> case deserialiseOrFail $ fromStrict b of
            Left _  -> throwError HiErrorInvalidArgument
            Right x -> return x
          _                -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunRead           ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueString s) -> return $ HiValueAction $ HiActionRead $ T.unpack s
          _                 -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunWrite          ->
      evalDoubleArgumentFunction args $ \arg1 arg2 ->
        case (arg1, arg2) of
          (HiValueString s, HiValueString s') -> do
            return $ HiValueAction $ HiActionWrite (T.unpack s) (TE.encodeUtf8 s')
          _                                   -> throwError HiErrorInvalidArgument


    HiValueFunction HiFunMkDir          ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueString s) -> return $ HiValueAction $ HiActionMkDir $ T.unpack s
          _                 -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunChDir          ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueString s) -> return $ HiValueAction $ HiActionChDir $ T.unpack s
          _                 -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunParseTime      ->
      evalSingleArgumentFunction args $ \arg1  ->
        case arg1 of
          (HiValueString s) -> do
            return $ case readMaybe $ T.unpack s :: Maybe UTCTime of
              Nothing -> HiValueNull
              Just t  -> HiValueTime t
          _                 -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunRand           ->
      evalDoubleArgumentFunction args $ \arg1 arg2 ->
        case (arg1, arg2) of
          (HiValueNumber (n1 :% 1), HiValueNumber (n2 :% 1)) -> do
            if n1 > n2
              then throwError HiErrorInvalidArgument
              else return $ HiValueAction $ HiActionRand (fromInteger n1) (fromInteger n2)
          _ -> throwError HiErrorInvalidArgument

    HiValueFunction HiFunEcho           ->
      evalSingleArgumentFunction args $ \arg1 -> do
        case arg1 of
          (HiValueString s) -> return $ HiValueAction $ HiActionEcho s
          _                 -> throwError HiErrorInvalidArgument

    HiValueList l                       ->
      evalSingleOrDoubleArgumentFunction args (\arg1 ->
        case arg1 of
          (HiValueNumber (n1 :% 1)) -> case Seq.lookup (fromInteger n1) l of
            Nothing -> return $ HiValueNull
            Just v  -> return $ v
          _                         -> throwError HiErrorInvalidArgument)
        (countRange Seq.empty l (Seq.length l) Seq.take Seq.drop HiValueList (\n obj g -> Seq.reverse $ g n $ Seq.reverse obj) Seq.take Seq.drop)

    HiValueString s                     ->
      evalSingleOrDoubleArgumentFunction args (\arg ->
        case arg of
          (HiValueNumber (n1 :% 1)) -> do
            return $ if n1 < 0 || n1 >= toInteger (T.length s)
              then HiValueNull
              else HiValueString $ T.singleton $ T.index s $ fromInteger n1
          _ -> throwError HiErrorInvalidArgument)
        (countRange (T.pack "") s (T.length s) T.take T.drop HiValueString (\n obj g -> g (abs n) obj) T.takeEnd T.dropEnd)

    HiValueBytes b                      ->
      evalSingleOrDoubleArgumentFunction args (\arg ->
        case arg of
          (HiValueNumber (n1 :% 1)) -> do
            return $ if n1 < 0 || n1 >= toInteger (B.length b)
              then HiValueNull
              else HiValueNumber $ fromIntegral $ B.index b $ fromInteger n1
          _ -> throwError HiErrorInvalidArgument)
        (countRange B.empty b (B.length b) B.take B.drop HiValueBytes (\n obj g -> g (B.length obj + n) b) B.drop B.take)
    _                                   -> throwError HiErrorInvalidFunction
  where
    countRange
      :: HiMonad m
      => a
      -> a
      -> Int
      -> (Int -> a -> a)
      -> (Int -> a -> a)
      -> (a -> HiValue)
      -> (Int -> a -> (Int -> a -> a) -> a)
      -> (Int -> a -> a)
      -> (Int -> a -> a)
      -> (HiValue -> HiValue -> ExceptT HiError m HiValue)
    countRange empty obj len takeFunc dropFunc modif f g h = \arg1 arg2 ->
      case (arg1, arg2) of
        (HiValueNull, HiValueNull)                      -> return $ modif obj
        (HiValueNumber (n :% 1), HiValueNull)           -> do
          let n1 = fromInteger n
          return $ modif $ if n1 < 0 then f n1 obj g else dropFunc n1 obj
        (HiValueNull, HiValueNumber (n :% 1))           -> do
          let n1 = fromInteger n
          return $ modif $ if n1 < 0 then f n1 obj h else takeFunc n1 obj
        (HiValueNumber (a :% 1), HiValueNumber (b :% 1)) -> do
          let n1 = fromInteger a
          let n2 = fromInteger b
          let start = if n1 < 0 then len + n1 else n1
          let end = if n2 < 0 then len + n2 else n2
          return $ modif $ if end < 0 || start >= len || start >= end
            then empty
            else takeFunc (end - start) $ dropFunc start obj
        _                                                -> throwError HiErrorInvalidArgument

    createDictFromHiValuesList :: [(HiValue, Integer)] -> HiValue
    createDictFromHiValuesList = HiValueDict . Map.map (HiValueNumber . toRational) . Map.fromListWith (+)

    applyStimesToMonoid
      :: (HiMonad m, Monoid a)
      => Rational
      -> a
      -> (a -> HiValue)
      -> ExceptT HiError m HiValue
    applyStimesToMonoid n x func = do
      case n of
        (n1 :% 1) -> if n1 > 0
          then return $ func $ n1 `stimes` x
          else throwError HiErrorInvalidArgument
        _         -> throwError HiErrorInvalidArgument

    equalValues ::(HiMonad m) => HiValue -> HiValue -> ExceptT HiError m Bool
    equalValues arg1 arg2 = return $ (==) arg1 arg2

    greaterValue :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m Bool
    greaterValue arg1 arg2 = case (arg1, arg2) of
      (HiValueBool _, HiValueNumber _) -> return $ False
      (HiValueNumber _, HiValueBool _) -> return $ True
      _                                -> return $ arg1 > arg2

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExpr
