{-# LANGUAGE TemplateHaskell #-}

module Multipost.Types
  ( module Multipost.UploadDestination.Qiita.Types
  , Arguments (..)
  , Article (..)
  , UploadDestination (..)
  , AccessToken
  , ArticleId
  , PostResult
  , Env (..)
  , UserRegex
  ) where


import           Data.Aeson.DeriveNoPrefix               (deriveJsonNoTypeNamePrefix)
import qualified Data.Text                               as T
import           Prelude                                 hiding (readFile,
                                                          writeFile)
import           Servant.Client                          (ClientError)

import           Multipost.UploadDestination.Qiita.Types


data Article = Article
  { articleBody  :: !T.Text
  , articleTags  :: ![Tag]
  , articleTitle :: !T.Text
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''Article)


type UserRegex = T.Text


data Arguments = Arguments
  { urlPlaceholderPattern :: UserRegex
  , titlePattern          :: UserRegex
  , tagsPattern           :: UserRegex
  , metadataPattern       :: UserRegex
  , qiitaAccessToken      :: T.Text
  , targetMarkdownPaths   :: [FilePath]
  } deriving Show


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
