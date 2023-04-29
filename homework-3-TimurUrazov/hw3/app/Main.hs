module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Set (fromList)
import Prettyprinter
import Prettyprinter.Util
import System.Console.Haskeline
import Text.Megaparsec.Error (errorBundlePretty)

import HW3.Action
import HW3.Base
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing    -> return ()
        Just ""    -> loop
        Just input -> do
          let parsedStr = parse input
          case parsedStr of
            Left x  -> do
              outputStr $ "parse error: "
              outputStrLn $ errorBundlePretty x
              loop
            Right x -> do
              evaluated <- liftIO $ runHIO (eval x) permissions
              case evaluated of
                Left y  -> do
                  outputStr "eval error: "
                  case y of
                    HiErrorInvalidArgument -> outputStrLn "invalid argument"
                    HiErrorInvalidFunction -> outputStrLn "invalid function"
                    HiErrorArityMismatch   -> outputStrLn "arity mismatch"
                    HiErrorDivideByZero    -> outputStrLn "divide by zero"
                  loop
                Right y -> do
                  liftIO $ putDocW 20 $ prettyValue y <+> (pretty "\n")
                  loop
    permissions = fromList [AllowRead, AllowWrite, AllowTime]
