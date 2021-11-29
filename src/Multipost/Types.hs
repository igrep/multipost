{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Multipost.Types
  ( module Multipost.UploadDestination.Qiita.Types
  , Arguments (..)
  , Metadata (..)
  , Preprocessor (..)
  , Article (..)
  , UploadDestination (..)
  , AccessToken
  , ArticleId
  , PostResult
  , Env (..)
  , YamlKey
  , parseQiitaTags
  ) where


import           Data.Aeson                              (FromJSON (parseJSON),
                                                          withObject, withText,
                                                          (.:), (.:?))
import           Data.Aeson.DeriveNoPrefix               (deriveJsonNoTypeNamePrefix)
import           Data.Bifunctor                          (Bifunctor (second),
                                                          first)
import           Data.Reflection                         (Given (given))
import qualified Data.Text                               as T
import           Data.Void                               (Void)
import           Prelude                                 hiding (readFile,
                                                          writeFile)
import           Servant.Client                          (ClientError)
import qualified Text.Megaparsec                         as M
import qualified Text.RE.TDFA.Text                       as Re

import           Data.Maybe                              (fromMaybe)
import           Multipost.UploadDestination.Qiita.Types


data Article = Article
  { articleBody  :: !T.Text
  , articleTags  :: ![Tag]
  , articleTitle :: !T.Text
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''Article)


type YamlKey = T.Text


data Arguments = Arguments
  { canonicalUrlKey     :: !YamlKey
  , titleKey            :: !YamlKey
  , tagsKey             :: !YamlKey
  , preprocessorsKey    :: !YamlKey
  , qiitaAccessToken    :: !T.Text
  , targetMarkdownPaths :: ![FilePath]
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
      M.label "Any character except '\\', '\\n', and 's'" $ M.noneOf "s\\\n"
    preprocessorRe <- M.manyTill (M.anySingleBut delim) (M.single delim)
    preprocessorReplacement <- M.manyTill (M.anySingleBut delim) (M.single delim)
    optsWithG <- M.many $ M.oneOf "gim"
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


parseQiitaTags :: T.Text -> [Tag]
parseQiitaTags =
  map
    ( uncurry Tag
      . second
        ( filter (not . T.null)
          . T.split (== ',')
          . T.drop 1 -- Drop the colon
        )
      . T.break (== ':'))
    . T.words


newtype CommaSeparatedTags =
  CommaSeparatedTags { fromCommaSeparatedTag :: [Tag] }
  deriving (Eq, Show)

instance FromJSON CommaSeparatedTags where
  parseJSON = withText "CommaSeparatedTags"
    $ pure . CommaSeparatedTags . parseQiitaTags


data Metadata = Metadata
  { metadataTitle         :: !T.Text
  , metadataTags          :: ![Tag]
  , metadataPreprocessors :: ![Preprocessor]
  , metadataCanonicalUrl  :: !(Maybe T.Text)
  } deriving Show

instance Given Arguments => FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \o -> do
    let Arguments { canonicalUrlKey, titleKey, tagsKey, preprocessorsKey } = given
    Metadata
      <$> o .: titleKey
      <*> (maybe [] fromCommaSeparatedTag <$> o .:? tagsKey)
      <*> (fromMaybe [] <$> o .:? preprocessorsKey)
      <*> o .:? canonicalUrlKey


data Env m = Env
  { qiita     :: !(UploadDestination m)
  , logDebug  :: String -> m ()
  , readFile  :: FilePath -> m T.Text
  , writeFile :: FilePath -> T.Text -> m ()
  }

type ArticleId = T.Text

type AccessToken = T.Text

-- NOTE: If I create a new upload destination, I should create a separate type for each upload destination
type PostResult = ItemResponse


data UploadDestination m = UploadDestination
  { postArticle  :: Article -> m (Either ClientError PostResult)
  , patchArticle :: ArticleId -> Article -> m (Either ClientError ())
  }
