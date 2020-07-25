module Main where

import qualified Data.Text.IO       as T
import           Prelude            hiding (readFile, writeFile)
import           System.Environment (getArgs)

import           Multipost

main :: IO ()
main = do
  qiitaUploadDestination <- mkQiitaUploadDestination
  let env = Env
        { qiita = qiitaUploadDestination
        , logDebug = putStrLn
        , readFile = T.readFile
        , writeFile = T.writeFile
        }
  mainWith env =<< getArgs
