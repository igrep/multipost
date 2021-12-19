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

  QiitaActions { postArticle, patchArticle } <- mkQiitaActions
  (command : left) <- getArgs
  case command of
      "items:post" -> do
        let qiitaArticleBody =
              "# Qiita APIテスト用新規投稿の見出し\nQiita APIテスト用新規投稿の本文"
            qiitaArticleTags =
              [ QiitaTag "QiitaAPI" ["2"]
              , QiitaTag "WebAPI" []
              ]
            qiitaArticleTitle = "Qiita APIテスト用新規投稿"
            qiitaArticle = QiitaArticle { qiitaArticleBody, qiitaArticleTags, qiitaArticleTitle }
        print =<< postArticle accessToken qiitaArticle
      "items:patch" -> do
        let qiitaArticleBody =
              "# Qiita APIテスト用記事（更新済み）の見出し\nQiita APIテスト用記事（更新済み）の本文"
            qiitaArticleTags =
              [ QiitaTag "QiitaAPI" ["2.1", "1.1"]
              , QiitaTag "WebAPI" ["1.3", "1.5"]
              ]
            qiitaArticleTitle = "Qiita APIテスト用記事（更新済み）"
            qiitaArticleId = T.pack $ head left
            qiitaArticle = QiitaArticle { qiitaArticleBody, qiitaArticleTags, qiitaArticleTitle }
        print =<< patchArticle accessToken qiitaArticleId qiitaArticle
      other ->
        fail $ "Unknown command: " ++ show other

{-

Example updated markdown file:
https://qiita.com/igrep/items/6c19aa64fe4731513af0.md

---
title: Qiita APIテスト用記事（更新済み）
tags: QiitaAPI:1.1,2.1 WebAPI:1.3,1.5
author: igrep
slide: false
---
# Qiita APIテスト用記事（更新済み）の見出し
Qiita APIテスト用記事（更新済み）の本文

-}
