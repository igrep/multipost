{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module Multipost.Types
  ( module Multipost.UploadDestination.Qiita.Types
  , Arguments (..)
  , Config (..)
  , Metadata (..)
  , Preprocessor (..)
  , Env (..)
  , YamlKey
  ) where


import           Data.Aeson                              (FromJSON (parseJSON),
                                                          withObject, withText,
                                                          (.:), (.:?))
import           Data.Bifunctor                          (first)
import           Data.Reflection                         (Given (given))
import qualified Data.Text                               as T
import qualified Data.Text.Lazy                          as TL
import           Data.Void                               (Void)
import qualified EnvParse.Applicative                    as EA
import           Prelude                                 hiding (readFile,
                                                          writeFile)
import qualified Text.Megaparsec                         as M
import qualified Text.RE.TDFA.Text                       as Re

import           Data.Maybe                              (fromMaybe)
import           Multipost.UploadDestination.Qiita.Types


type YamlKey = T.Text


data Arguments = Arguments
  { printExampleDotEnv  :: Bool
  , dotEnvFiles         :: [FilePath]
  , targetMarkdownPaths :: [FilePath]
  } deriving (Eq, Show)


data Config = Config
  { qiitaAccessToken    :: QiitaAccessToken
  , zennRepository      :: FilePath
  , preprocessorsKey    :: Maybe YamlKey
  , qiitaTagsKey        :: Maybe YamlKey
  , canonicalUrlKey     :: YamlKey
  , canonicalServiceKey :: YamlKey
  , titleKey            :: YamlKey
  } deriving Show


data Preprocessor = Preprocessor
  { preprocessorRe          :: String
  , preprocessorReplacement :: String
  , preprocessorReOpts      :: Re.SimpleREOptions
  , preprocessorGlobal      :: Bool
  } deriving (Eq, Show)

instance FromJSON Preprocessor where
  parseJSON = withText "Preprocessor" $
    either fail pure . parseSubstituteCommand


parseSubstituteCommand :: T.Text -> Either String Preprocessor
parseSubstituteCommand =
  first M.errorBundlePretty . M.runParser p "Preprocessor"
 where
  p :: M.Parsec Void T.Text Preprocessor
  p = do
    _ <- M.optional $ M.single 's'

    -- Ref. https://pubs.opengroup.org/onlinepubs/009604599/utilities/sed.html
    -- > Any character other than backslash or <newline> can be used instead of a slash to delimit the BRE and the replacement.
    -- But I add "s" to the exception to avoid ambiguity with the initial 's' character.
    delim <-
      M.label "Any characters except '\\', '\\n', and 's'" $ M.noneOf ("s\\\n" :: String)
    preprocessorRe <- M.manyTill (M.anySingleBut delim) (M.single delim)
    preprocessorReplacement <- M.manyTill (M.anySingleBut delim) (M.single delim)
    optsWithG <- M.many $ M.oneOf ("gim" :: String)
    let preprocessorGlobal = 'g' `elem` optsWithG
        preprocessorReOpts =
          case ('m' `elem` optsWithG, 'i' `elem` optsWithG) of
              (True, True)   -> Re.MultilineInsensitive
              (True, False)  -> Re.MultilineSensitive
              (False, True)  -> Re.BlockInsensitive
              (False, False) -> Re.BlockSensitive
    pure Preprocessor
      { preprocessorRe
      , preprocessorReplacement
      , preprocessorReOpts
      , preprocessorGlobal
      }


data Metadata = Metadata
  { metadataTitle            :: T.Text
  , metadataQiitaTags        :: [QiitaTag]
  , metadataPreprocessors    :: [Preprocessor]
  , metadataCanonicalUrl     :: Maybe T.Text
  , metadataCanonicalService :: Maybe Service
  } deriving Show

instance Given Config => FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \o ->
    Metadata
      <$> o .: titleKey
      <*> qiitaTagsOf o
      <*> preprocessorsOf o
      <*> o .:? canonicalUrlKey
      <*> o .:? canonicalServiceKey
   where
    Config { canonicalUrlKey, canonicalServiceKey, preprocessorsKey, titleKey, qiitaTagsKey } = given

    qiitaTagsOf o =
      case qiitaTagsKey of
          Nothing  -> pure []
          Just key -> maybe [] fromCommaSeparatedTag <$> o .:? key

    preprocessorsOf o =
      case preprocessorsKey of
          Nothing  -> pure []
          Just key -> fromMaybe [] <$> o .:? key


data Env m = Env
  { qiitaActions :: QiitaActions m
  , logDebug     :: String -> m ()
  , readFile     :: FilePath -> m T.Text
  , writeFile    :: FilePath -> T.Text -> m ()
  , loadDotEnv   :: FilePath -> m ()
  , decodeEnv    :: EA.ToEnvVarName -> EA.CodecEnv Config -> m Config
  , puts         :: TL.Text -> m ()
  }

data Service = Qiita | Zenn deriving (Eq, Show)

instance FromJSON Service where
  parseJSON = withText "Service" $ \case
      "qiita" -> pure Qiita
      "zenn"  -> pure Zenn
      other -> fail $ "Unexpected service: " ++ T.unpack other
