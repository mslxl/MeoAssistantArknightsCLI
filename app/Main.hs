module Main where

import Control.Monad (forever)
import qualified JsonParser as Json
import qualified Maa as M
import System.Exit

exitIfFalse :: Bool -> IO ()
exitIfFalse True = return ()
exitIfFalse False = exitFailure

main :: IO ()
main = do
  --Note: Test only
  --A argument parser will be here soon

  M.maaVersion >>= putStrLn . ("MeoAssistant " ++)
  asst <- M.maaAsstCreate
  let tk = M.maaConnect "192.168.56.101:5555" <> M.maaWakeup <> M.maaStart
  M.runTask tk asst >>= exitIfFalse . snd
  forever getLine
