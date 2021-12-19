{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Multipost.UploadDestination.Qiita
  ( module Multipost.UploadDestination.Qiita.Types
  , mkQiitaActions
  ) where


import           Data.Functor                            (void)
import           Data.Proxy
import qualified Data.Text                               as T
import           Network.HTTP.Client.TLS                 (newTlsManager)
import           Servant.API                             ((:<|>) ((:<|>)), (:>),
                                                          Capture, Header',
                                                          JSON, Patch, Post,
                                                          ReqBody, Required,
                                                          Strict)
import           Servant.Client                          (ClientM, client,
                                                          mkClientEnv,
                                                          parseBaseUrl,
                                                          runClientM)

import           Multipost.Types
import           Multipost.UploadDestination.Qiita.Types

type AuthorizationHeader = T.Text

type Auth = Header' '[Required, Strict] "Authorization" AuthorizationHeader

type Api =
       Auth :> "items" :> ReqBody '[JSON] PostItemRequest :> Post '[JSON] ItemResponse
  :<|> Auth :> "items" :> Capture "item_id" ItemId :> ReqBody '[JSON] PatchItemRequest :> Patch '[JSON] ItemResponse


api :: Proxy Api
api = Proxy


postItem :: AuthorizationHeader -> PostItemRequest -> ClientM ItemResponse
patchItem :: AuthorizationHeader -> ItemId -> PatchItemRequest -> ClientM ItemResponse
postItem :<|> patchItem = client api


mkQiitaActions :: IO (QiitaActions IO)
mkQiitaActions = do
  clientEnv <- mkClientEnv <$> newTlsManager <*> parseBaseUrl "https://qiita.com/api/v2/"
  let run = (`runClientM` clientEnv)

      postArticle accessToken QiitaArticle { qiitaArticleBody, qiitaArticleTags, qiitaArticleTitle } = do
        run . postItem ("Bearer " <> accessToken) $
          PostItemRequest qiitaArticleBody qiitaArticleTags qiitaArticleTitle False

      patchArticle accessToken articleId QiitaArticle { qiitaArticleBody, qiitaArticleTags, qiitaArticleTitle } =
        run . void . patchItem ("Bearer " <> accessToken) articleId $
          PatchItemRequest qiitaArticleBody qiitaArticleTags qiitaArticleTitle

  return QiitaActions { postArticle, patchArticle }
