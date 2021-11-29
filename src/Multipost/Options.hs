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
  canonicalUrlKey <-
    strOption (long "canonical-url" <> metavar "YAML_KEY") <|> pure "canonical-url"
  titleKey <-
    strOption (long "title" <> metavar "YAML_KEY") <|> pure "title"
  tagsKey <-
    strOption (long "tags" <> metavar "YAML_KEY") <|> pure "qiitaTags"
  preprocessorsKey <-
    strOption (long "qiita-preprocessors" <> metavar "YAML_KEY") <|> pure "qiita-preprocessors"
  qiitaAccessToken <-
    strOption (long "qiita-access-token" <> metavar "QIITA_ACCESS_TOKEN")
  targetMarkdownPaths <- many . strArgument $ metavar "FILES_TO_UPLOAD"
  pure Arguments {..}


parserInfo :: ParserInfo Arguments
parserInfo = info
  (helper <*> parser)
  (fullDesc <> header "Upload markdown file(s) onto Qiita")


fromArgs :: IO Arguments
fromArgs = execParser parserInfo
