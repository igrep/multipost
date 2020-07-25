{-# LANGUAGE TemplateHaskell #-}

module Multipost.UploadDestination.Qiita.Types where


import           Data.Aeson.DeriveNoPrefix (deriveJsonNoTypeNamePrefix)
import qualified Data.Text                 as T


type ItemId = T.Text


data Tag = Tag
  { tagName     :: !T.Text
  , tagVersions :: ![T.Text]
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''Tag)


data PostItemRequest = PostItemRequest
  { postItemRequestBody  :: !T.Text
  , postItemRequestTags  :: ![Tag]
  , postItemRequestTitle :: !T.Text
  , postItemRequestTweet :: !Bool
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''PostItemRequest)


data PatchItemRequest = PatchItemRequest
  { patchItemRequestBody  :: !T.Text
  , patchItemRequestTags  :: ![Tag]
  , patchItemRequestTitle :: !T.Text
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''PatchItemRequest)


data ItemResponse = ItemResponse
  { itemResponseId  :: !ItemId
  , itemResponseUrl :: !T.Text
  } deriving (Eq, Show)

$(deriveJsonNoTypeNamePrefix ''ItemResponse)
