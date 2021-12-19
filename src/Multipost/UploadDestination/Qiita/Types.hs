{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module Multipost.UploadDestination.Qiita.Types where


import           Data.Aeson                (FromJSON (parseJSON), withText)
import           Data.Aeson.DeriveNoPrefix (deriveJsonNoTypeNamePrefix)
import           Data.Bifunctor            (Bifunctor (second))
import qualified Data.Text                 as T
import           Servant.Client            (ClientError)


data QiitaTag = QiitaTag
  { qiitaTagName     :: T.Text
  , qiitaTagVersions :: [T.Text]
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''QiitaTag)


data QiitaArticle = QiitaArticle
  { qiitaArticleBody  :: T.Text
  , qiitaArticleTags  :: [QiitaTag]
  , qiitaArticleTitle :: T.Text
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''QiitaArticle)


type ItemId = T.Text


data PostItemRequest = PostItemRequest
  { postItemRequestBody  :: T.Text
  , postItemRequestTags  :: [QiitaTag]
  , postItemRequestTitle :: T.Text
  , postItemRequestTweet :: Bool
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''PostItemRequest)


data PatchItemRequest = PatchItemRequest
  { patchItemRequestBody  :: T.Text
  , patchItemRequestTags  :: [QiitaTag]
  , patchItemRequestTitle :: T.Text
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''PatchItemRequest)


data ItemResponse = ItemResponse
  { itemResponseId  :: ItemId
  , itemResponseUrl :: T.Text
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''ItemResponse)

type QiitaArticleId = T.Text


data QiitaActions m = QiitaActions
  { postArticle  :: QiitaAccessToken -> QiitaArticle -> m (Either ClientError ItemResponse)
  , patchArticle :: QiitaAccessToken -> QiitaArticleId -> QiitaArticle -> m (Either ClientError ())
  }


parseQiitaTags :: T.Text -> [QiitaTag]
parseQiitaTags =
  map
    ( uncurry QiitaTag
      . second
        ( filter (not . T.null)
          . T.split (== ',')
          . T.drop 1 -- Drop the colon
        )
      . T.break (== ':'))
    . T.words


newtype CommaSeparatedTags =
  CommaSeparatedTags { fromCommaSeparatedTag :: [QiitaTag] }
  deriving (Eq, Show)

instance FromJSON CommaSeparatedTags where
  parseJSON = withText "CommaSeparatedTags"
    $ pure . CommaSeparatedTags . parseQiitaTags

type QiitaAccessToken = T.Text
