{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module Multipost.Options
  ( fromArgs
  , parser
  , parserInfo
  ) where


import           Options.Applicative

import           Multipost.Types


parser :: Parser Arguments
parser = do
  urlPlaceholderPattern <-
    strOption (long "url-placeholder" <> metavar "REGEX")
  titlePattern <-
    strOption (long "title" <> metavar "REGEX")
  tagsPattern <-
    strOption (long "tags" <> metavar "REGEX")
  metadataPattern <-
    strOption (long "metadata" <> metavar "REGEX")
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
