{-# LANGUAGE LambdaCase #-}

module HW3.Action
  ( HIO(..)
  , HiPermission(..)
  , PermissionException(..)
  ) where

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Sequence as Seq
import Data.Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import System.Directory
import System.Random.Stateful (getStdRandom, uniformR)

import HW3.Base

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

data PermissionException =
  PermissionRequired HiPermission
  deriving (Show, Eq, Ord)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap f (HIO g) = HIO $ fmap f . g

instance Applicative HIO where
  pure = HIO . const . pure
  HIO f <*> HIO g = HIO $ \s -> f s <*> g s

instance Monad HIO where
  return = HIO . const . return
  HIO f >>= g = HIO $ \s -> f s >>= \x -> runHIO (g x) s

instance MonadIO HIO where
  liftIO = HIO . const

instance HiMonad HIO where
  runAction = \case
    HiActionRead path -> checkAndPerfrormAction AllowRead $ do
      isDir <- liftIO $ doesDirectoryExist path
      if isDir
        then do
          files <- liftIO $ listDirectory path
          return $ HiValueList $ Seq.fromList $ (HiValueString . T.pack) <$> files
        else do
          exists <- liftIO $ doesFileExist path
          if exists
            then do
              contents <- liftIO $ B.readFile path
              case TE.decodeUtf8' contents of
                Left _ -> return $ HiValueBytes contents
                Right x -> return $ HiValueString x
            else
              return HiValueNull
    HiActionWrite path contents -> checkAndPerfrormAction AllowWrite $ do
      liftIO $ B.writeFile path contents
      return HiValueNull
    HiActionMkDir path -> checkAndPerfrormAction AllowWrite $ do
      liftIO $ createDirectory path
      return HiValueNull
    HiActionChDir path -> checkAndPerfrormAction AllowRead $ do
      liftIO $ setCurrentDirectory path
      return HiValueNull
    HiActionCwd -> checkAndPerfrormAction AllowRead $ do
      path <- liftIO getCurrentDirectory
      return $ HiValueString $ T.pack path
    HiActionNow -> checkAndPerfrormAction AllowTime $ do
      time <- liftIO getCurrentTime
      return $ HiValueTime time
    HiActionRand from to -> (getStdRandom $ uniformR (from, to)) >>= return . HiValueNumber . toRational
    HiActionEcho text -> checkAndPerfrormAction AllowWrite $ do
      liftIO $ putStrLn $ T.unpack text
      return HiValueNull
    where
      askPermission :: HIO (Set HiPermission)
      askPermission = HIO $ \s -> return s

      checkAndPerfrormAction :: HiPermission -> HIO HiValue -> HIO HiValue
      checkAndPerfrormAction permission action = do
        permissions <- askPermission
        if permission `member` permissions
          then action
          else liftIO $ throwIO $ PermissionRequired permission
