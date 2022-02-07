module Main where

import qualified JsonParser as Json
import qualified Maa as M

main :: IO ()
main = ("MeoAssistant " ++) <$> M.maaVersion >>= putStrLn
