module Main where

import Control.Monad (forever)
import Data.Default
import qualified JsonParser as Json
import Maa (FightConfig, RecruitConfig (maaRecruitMaxTimes))
import qualified Maa as M
import System.Exit

exitIfFalse :: Bool -> IO ()
exitIfFalse True = return ()
exitIfFalse False = exitFailure

main :: IO ()
main = do
  --Note: Test only
  --A argument parser will be here soon

  M.version >>= putStrLn . ("MeoAssistant " ++)
  asst <- M.create
  let tk =
        M.connect "192.168.56.101:5555"
          <> M.wakeup
          <> M.fight def
          <> M.recruit def {maaRecruitMaxTimes = 4}
          <> M.infrast def
          <> M.visit
          <> M.mall True
          <> M.award
          <> M.start
  M.runTask tk asst >>= exitIfFalse . snd
  forever getLine
