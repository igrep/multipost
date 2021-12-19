{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Multipost.Options
  ( fromArgs
  , parser
  , parserInfo
  ) where


import           Options.Applicative

import           Multipost.Types


parser :: Parser Arguments
parser = do
  printExampleDotEnv <- switch
    $ long "print-example-dot-env"
    <> help "Print out an example .env file, then exit."
  dotEnvFiles <-
    fmap (defaultIfEmpty [".env", ".secret.env"])
      . many
      . strOption
      $ long "dotenv"
      <> help "Path to dotenv file(s) to modify the environment variables. Default \".env\" and \".secret.env\"."
  targetMarkdownPaths <- many . strArgument
    $ metavar "FILES_TO_UPLOAD"
    <> help "Target markdown files to upload on Qiita or Zenn."
  pure Arguments {..}
 where
  defaultIfEmpty def vs = if null vs then def else vs


parserInfo :: ParserInfo Arguments
parserInfo = info
  (helper <*> parser)
  (fullDesc <> header "Upload markdown file(s) onto Qiita or Zenn.")


fromArgs :: IO Arguments
fromArgs = execParser parserInfo
