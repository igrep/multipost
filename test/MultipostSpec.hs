{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module MultipostSpec
  ( main, spec
  ) where


import           Control.Exception          (SomeException (SomeException))
import           Control.Monad.Catch.Pure   (CatchT, runCatchT)
import           Control.Monad.State.Strict (State, gets, modify', runState)
import           Control.Monad.Trans        (lift)
import           Data.Bifunctor             (first)
import           Data.Either                (isLeft)
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Test.FileSystem.Fake       (FileSystem, FileSystemT, readPath,
                                             runFileSystemT, writePath)
import           Test.Syd
import           Text.Nowdoc                (nowdoc)

import           Prelude                    hiding (readFile, writeFile)

import           Debug.Trace                (traceM)

import           Multipost


main :: IO ()
main = sydTest spec


type UploadDestinationState = M.Map QiitaArticleId (Maybe QiitaArticle)


type TestM = CatchT (FileSystemT T.Text (State UploadDestinationState))


runTestM
  :: TestM a
  -> (FileSystem T.Text, UploadDestinationState, Either String a)
runTestM action =
  let ((result, fs), uds) =
        (`runState` mkInitialUploadDestinationState articleIds)
          . (`runFileSystemT` initialFs)
          $ runCatchT action
   in (fs, uds, first (show . SomeException) result)


mkInitialUploadDestinationState :: [QiitaArticleId] -> UploadDestinationState
mkInitialUploadDestinationState = M.fromList . map (\articleId -> (articleId, Nothing))


testEnv :: Env TestM
testEnv = Env
  { qiitaActions = QiitaActions { postArticle, patchArticle }
  , logDebug = traceM
  , puts = traceM . TL.unpack
  , loadDotEnv = \_ -> return ()
  , decodeEnv = \_ _ -> return Config
      { canonicalUrlKey = "canonical-url"
      , canonicalServiceKey = "canonical-service"
      , zennRepository = "zenn"
      , titleKey = "title"
      , qiitaTagsKey = Just "qiita-tags"
      , qiitaAccessToken = "qiitaAccessToken should not be used in test"
      , preprocessorsKey = Just "preprocessors"
      }
  , readFile = readPath
  , writeFile = writePath
  }
 where
  postArticle _accessToken article = do
    itemResponseId <- lift . lift . gets $ fst . M.findMin
    lift . lift . modify' . M.insert itemResponseId $ Just article
    let itemResponseUrl = "https://qiita.example.com/" <> itemResponseId
    return $ Right ItemResponse { itemResponseId, itemResponseUrl }

  patchArticle _accessToken articleId article = do
    lift . lift . modify' . M.insert articleId $ Just article
    return $ Right ()


testArgumentsAgainst :: FilePath -> Arguments
testArgumentsAgainst targetPath = Arguments
  { printExampleDotEnv = False
  , dotEnvFiles = ["not_used.env"]
  , targetMarkdownPaths = [targetPath]
  }


initialFs :: FileSystem T.Text
initialFs = M.fromList
  [ ("urlPlaceholderEmpty", urlPlaceholderEmpty)
  , ("urlPlaceholderOnlyQiita", urlPlaceholderOnlyQiita)
  , ("urlPlaceholderInvalid", urlPlaceholderInvalid)
  , ("urlPlaceholderFilled", urlPlaceholderFilled)
  , ("noUrlPlaceholderLine", noUrlPlaceholderLine)
  ]


urlPlaceholderEmpty :: T.Text
urlPlaceholderEmpty = [nowdoc|
---
title: 𠮷野家に行ってきました
author: YAMAMOTO Yuji
date: April 16, 2020
canonical-url:
canonical-service: qiita
qiita-tags: tag1:1.1 tag2:1.2
preprocessors:
- s/  $//
...
---

Test of surrogate pair: 𠮷野家は美味しい
Yoshino-ya  is  good!
|]


urlPlaceholderUpdated :: T.Text
urlPlaceholderUpdated = [nowdoc|
---
title: 𠮷野家に行ってきました
author: YAMAMOTO Yuji
date: April 16, 2020
canonical-url: https://qiita.example.com/article1
canonical-service: qiita
qiita-tags: tag1:1.1 tag2:1.2
preprocessors:
- s/  $//
...
---

Test of surrogate pair: 𠮷野家は美味しい
Yoshino-ya  is  good!
|]


urlPlaceholderOnlyQiita :: T.Text
urlPlaceholderOnlyQiita = [nowdoc|
---
title: 𠮷野家に行ってきました
author: YAMAMOTO Yuji
date: April 16, 2020
canonical-url: qiita
canonical-service: qiita
qiita-tags: tag1:1.1 tag2:1.2
preprocessors:
- s/  $//
...
---

Test of surrogate pair: 𠮷野家は美味しい
Yoshino-ya  is  good!
|]


urlPlaceholderInvalid :: T.Text
urlPlaceholderInvalid = [nowdoc|
---
title: 𠮷野家に行ってきました
author: YAMAMOTO Yuji
date: April 16, 2020
canonical-url: https://qiita.com/no_item_id/
canonical-service: qiita
qiita-tags: tag1:1.1 tag2:1.2
...
---

Test of surrogate pair: 𠮷野家は美味しい
Yoshino-ya is good!
|]


urlPlaceholderFilled :: T.Text
urlPlaceholderFilled = [nowdoc|
---
title: 𠮷野家に行ってきました (Updated)
author: YAMAMOTO Yuji
date: April 16, 2020
canonical-url: https://qiita.com/user_id/items/article2/
canonical-service: qiita
qiita-tags: tag1:2.0 tag2:3.1 tag3
preprocessors:
- s/  $//
...
---

(Updated)

Test of surrogate pair: 𠮷野家は超美味しい
Yoshino-ya  is  so good!
|]


noUrlPlaceholderLine :: T.Text
noUrlPlaceholderLine = [nowdoc|
---
title: 𠮷野家に行ってきました
author: YAMAMOTO Yuji
date: April 16, 2020
...
---

Test of surrogate pair: 𠮷野家は美味しい
Yoshino-ya is good!
|]

articleId1, articleId2 :: T.Text
articleId1 = "article1"
articleId2 = "article2"

articleIds :: [T.Text]
articleIds = [articleId1, articleId2]


spec :: Spec
spec =
  describe "Multipost" $
    describe "mainWith" $ do
      let subject targetPath =
            runTestM
              . mainWith testEnv
              $ testArgumentsAgainst targetPath

      describe "Given a markdown file including a line matched the --url-placeholder option" $ do
        describe "the URL in the matched line is empty" $
          it "do nothing" $ do
            let uds = mkInitialUploadDestinationState articleIds
                (fsActual, udsActual, result) = subject "urlPlaceholderEmpty"
            either expectationFailure return result
            fsActual `shouldBe` initialFs
            udsActual `shouldBe` uds

        describe "the URL in the matched line is just 'qiita'" $
          it "uploads the article, write the URL returned by Qiita" $ do
            let filePath = "urlPlaceholderOnlyQiita"
                fsUpdated = M.insert filePath urlPlaceholderUpdated initialFs

                udsUpdated = M.fromList [(articleId1, Just article), (articleId2, Nothing)]
                article = QiitaArticle body tags title
                body = "Test of surrogate pair: 𠮷野家は美味しい\nYoshino-ya  is  good!"
                tags = [QiitaTag "tag1" ["1.1"], QiitaTag "tag2" ["1.2"]]
                title = "𠮷野家に行ってきました"

                (fsActual, udsActual, result) = subject filePath
            either expectationFailure return result
            udsActual `shouldBe` udsUpdated
            fsActual `shouldBe` fsUpdated

        describe "the URL in the matched line is invalid" $
          it "throws an error without updating anything" $ do
            let uds = mkInitialUploadDestinationState articleIds
                (fsActual, udsActual, result) = subject "urlPlaceholderInvalid"
            fsActual `shouldBe` initialFs
            udsActual `shouldBe` uds
            result `shouldSatisfy` isLeft

        describe "the URL in the matched line is filled with the URL with an articleId." $
          it "updates the exisiting article on Qiita" $ do
            let udsUpdated = M.fromList [(articleId1, Nothing), (articleId2, Just article)]
                article = QiitaArticle body tags title
                body = "(Updated)\n\nTest of surrogate pair: 𠮷野家は超美味しい\nYoshino-ya  is  so good!"
                tags = [QiitaTag "tag1" ["2.0"], QiitaTag "tag2" ["3.1"], QiitaTag "tag3" []]
                title = "𠮷野家に行ってきました (Updated)"
                (fsActual, udsActual, result) = subject "urlPlaceholderFilled"
            either expectationFailure return result
            fsActual `shouldBe` initialFs
            udsActual `shouldBe` udsUpdated

      describe "Given a markdown file NOT including a line matched the --url-placeholder option" $
        it "do nothing" $ do
          let uds = mkInitialUploadDestinationState articleIds
              (fsActual, udsActual, result) = subject "noUrlPlaceholderLine"
          either expectationFailure return result
          fsActual `shouldBe` initialFs
          udsActual `shouldBe` uds
