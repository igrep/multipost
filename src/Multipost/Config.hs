{-# LANGUAGE OverloadedStrings #-}

module Multipost.Config where


import           EnvParse.Applicative

import           Multipost.Types


codec :: CodecEnv Config
codec = Config
  <$> envRequired "qiitaAccessToken" stringVal "Access token for Qiita"
  <*> envRequired "zennRepository" stringVal "Path to the repository containing your articles on Zenn."
  <*> envOptional "preprocessorsKey" stringVal "In which metadata key the article's preprocessors are defined"
  <*> envOptional "qiitaTags" stringVal "In which metadata key the article's tags on Qiita are defined"
  <*> envWithDefault "canonicalUrlKey" stringVal "canonical-url" "In which metadata key the article's canonical URL is defined"
  <*> envWithDefault "canonicalServiceKey" stringVal "canonical-service" "In which metadata key the article's canonical service is defined"
  <*> envWithDefault "titleKey" stringVal "title" "In which metadata key the article's title is defined"


toEnvVarName :: ToEnvVarName
toEnvVarName = toUpperSnakePrefixedWith "multipost"
