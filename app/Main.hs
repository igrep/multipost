{-# LANGUAGE NamedFieldPuns #-}

module Main where


import qualified Data.Text.IO      as T
import           Prelude           hiding (readFile, writeFile)

import           Multipost
import qualified Multipost.Options as Opt


main :: IO ()
main = do
  args <- Opt.fromArgs
  qiita <- mkQiitaUploadDestination $ qiitaAccessToken args
  let env = Env
        { qiita
        , logDebug = putStrLn
        , readFile = T.readFile
        , writeFile = T.writeFile
        }
  mainWith env args
