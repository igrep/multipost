{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Text.IO         as T
import qualified Data.Text.Lazy.IO    as TL
import qualified EnvParse.Applicative as EA
import           LoadEnv              (loadEnvFromAbsolute)
import           Prelude              hiding (readFile, writeFile)

import           Multipost
import qualified Multipost.Options    as Opt


main :: IO ()
main = do
  args <- Opt.fromArgs
  qiitaActions <- mkQiitaActions
  let env = Env
        { qiitaActions
        , loadDotEnv = loadEnvFromAbsolute
        , decodeEnv = EA.decodeEnvThrow
        , puts = TL.putStrLn
        , logDebug = putStrLn
        , readFile = T.readFile
        , writeFile = T.writeFile
        }
  mainWith env args
