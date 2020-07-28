{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Environment (getArgs)
import           System.IO          (BufferMode (NoBuffering), hSetBuffering,
                                     hSetEcho, stdin, stdout)

import           Multipost

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Qiita Access Token: "
  hSetEcho stdin False
  accessToken <- T.getLine

  qiita <- mkQiitaUploadDestination accessToken
  (command : left) <- getArgs
  case command of
      "items:post" -> do
        let articleBody =
              "# Qiita APIテスト用新規投稿の見出し\nQiita APIテスト用新規投稿の本文"
            articleTags =
              [ Tag "QiitaAPI" ["2"]
              , Tag "WebAPI" []
              ]
            articleTitle = "Qiita APIテスト用新規投稿"
            article = Article { articleBody, articleTags, articleTitle }
        print =<< postArticle qiita article
      "items:patch" -> do
        let articleBody =
              "# Qiita APIテスト用記事（更新済み）の見出し\nQiita APIテスト用記事（更新済み）の本文"
            articleTags =
              [ Tag "QiitaAPI" ["2"]
              , Tag "WebAPI" ["newVersion"]
              ]
            articleTitle = "Qiita APIテスト用記事（更新済み）"
            articleId = T.pack $ head left
            article = Article { articleBody, articleTags, articleTitle }
        print =<< patchArticle qiita articleId article
      other ->
        fail $ "Unknown command: " ++ show other
