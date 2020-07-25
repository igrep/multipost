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
  , ShownRe (..)
  )where


import           Data.Aeson.DeriveNoPrefix               (deriveJsonNoTypeNamePrefix)
import qualified Data.Text                               as T
import           Prelude                                 hiding (readFile,
                                                          writeFile)
import           Servant.Client                          (ClientError)
import           Text.RE.TDFA.Text                       (RE, reSource)

import           Multipost.UploadDestination.Qiita.Types


data Article = Article
  { articleBody  :: !T.Text
  , articleTags  :: ![Tag]
  , articleTitle :: !T.Text
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''Article)


newtype ShownRe = ShownRe { unShownRe :: RE }

instance Show ShownRe where
  show (ShownRe re) = "\"" ++ reSource re ++ "\""


data Arguments = Arguments
  { urlPlaceholderPattern :: ShownRe
  , titlePattern          :: ShownRe
  , tagsPattern           :: ShownRe
  , metadataPattern       :: ShownRe
  , qiitaAccessToken      :: T.Text
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
  { postArticle  :: AccessToken -> Article -> m (Either ClientError PostResult)
  , patchArticle :: AccessToken -> ArticleId -> Article -> m (Either ClientError ())
  }
