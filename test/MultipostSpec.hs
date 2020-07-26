{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module MultipostSpec
  ( main, spec
  ) where


import           Control.Exception          (IOException, SomeException)
import           Control.Monad.Catch.Pure   (CatchT, runCatchT)
import           Control.Monad.State.Strict (StateT, gets, modify', runStateT)
import           Control.Monad.Trans        (lift)
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import           Test.FileSystem.Fake       (FileSystem, FileSystemM, readPath,
                                             runFileSystemM, writePath)
import           Test.Hspec
import           Text.Nowdoc                (nowdoc)

import           Prelude                    hiding (readFile, writeFile)

import           Debug.Trace                (traceM)

import           Multipost


main :: IO ()
main = hspec spec


type UploadDestinationState = M.Map ArticleId (Maybe Article)


type TestM = CatchT (StateT UploadDestinationState (FileSystemM T.Text))


runTestM
  :: TestM a
  -> (FileSystem T.Text, (Either IOException (Either SomeException a, UploadDestinationState)))
runTestM =
  runFileSystemM initialFs . (`runStateT` mkInitialUploadDestinationState articleIds) . runCatchT


mkInitialUploadDestinationState :: [ArticleId] -> UploadDestinationState
mkInitialUploadDestinationState = M.fromList . map (\articleId -> (articleId, Nothing))


testEnv :: Env TestM
testEnv = Env
  { qiita
  , logDebug = traceM
  , readFile = lift . lift . readPath
  , writeFile = \path -> lift . lift . writePath path
  }
 where
  qiita = UploadDestination { postArticle, patchArticle }

  postArticle _accessToken article = do
    itemResponseId <- lift . gets $ fst . M.findMin
    lift . modify' . M.insert itemResponseId $ Just article
    let itemResponseUrl = "https://qiita.example.com/" <> itemResponseId
    return $ Right ItemResponse { itemResponseId, itemResponseUrl }

  patchArticle _accessToken articleId article = do
    lift . modify' . M.insert articleId $ Just article
    return $ Right ()


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
qiita-tags: tag1:1 tag2:2
...
---

Test of surrogate pair: 𠮷野家は美味しい
Yoshino-ya is good!
|]


urlPlaceholderEmptyUpdated :: T.Text
urlPlaceholderEmptyUpdated = [nowdoc|
---
title: 𠮷野家に行ってきました
author: YAMAMOTO Yuji
date: April 16, 2020
canonical-url: https://qiita.example.com/article1
qiita-tags: tag1:1.1 tag2:1.2
...
---

Test of surrogate pair: 𠮷野家は美味しい
Yoshino-ya is good!
|]


urlPlaceholderOnlyQiita :: T.Text
urlPlaceholderOnlyQiita = [nowdoc|
---
title: 𠮷野家に行ってきました
author: YAMAMOTO Yuji
date: April 16, 2020
canonical-url: qiita
qiita-tags: tag1:1.1 tag2:1.2
...
---

Test of surrogate pair: 𠮷野家は美味しい
Yoshino-ya is good!
|]


urlPlaceholderInvalid :: T.Text
urlPlaceholderInvalid = [nowdoc|
---
title: 𠮷野家に行ってきました
author: YAMAMOTO Yuji
date: April 16, 2020
canonical-url: https://qiita.com/no_item_id/
qiita-tags: tag1:1 tag2:2
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
qiita-tags: tag1:2.0 tag2:3.1 tag3
...
---

(Updated)

Test of surrogate pair: 𠮷野家は超美味しい
Yoshino-ya is so good!
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
              $ ["--url-placeholder", "^canonical-url:(.*)$"]
                ++ ["--title", "^title:(.*)$"]
                ++ ["--tags", "^qiita-tags:(.*)$"]
                ++ ["--metadata", "^---.+---"]
                ++ ["--qiita-access-token", "ignored_in_test"]
                ++ [targetPath]

          itShouldUploadsArticleAndWriteTheUrlReturnedByQiita
            :: HasCallStack => FilePath -> Spec
          itShouldUploadsArticleAndWriteTheUrlReturnedByQiita filePath =
            it "uploads the article, write the URL returned by Qiita" $ do
              let fsUpdated = M.insert "urlPlaceholderEmpty" urlPlaceholderEmptyUpdated initialFs

                  udsUpdated = M.fromList [(articleId1, Just article), (articleId2, Nothing)]
                  article = Article body tags title
                  body = "Test of surrogate pair: 𠮷野家は美味しい\nYoshino-ya is good!\n"
                  tags = [Tag "tag1" ["1.1"], Tag "tag2" ["1.2"]]
                  title = "𠮷野家に行ってきました"

                  (fsActual, Right (Right (), udsActual)) = subject filePath
              fsActual `shouldBe` fsUpdated
              udsActual `shouldBe` udsUpdated

      context "Given a markdown file including a line matched the --url-placeholder option" $ do
        context "the URL in the matched line is empty" $
          itShouldUploadsArticleAndWriteTheUrlReturnedByQiita "urlPlaceholderEmpty"

        context "the URL in the matched line is just 'qiita'" $
          itShouldUploadsArticleAndWriteTheUrlReturnedByQiita "urlPlaceholderOnlyQiita"

        context "the URL in the matched line is invalid" $
          it "throws an error without updating anything" $ do
            let uds = mkInitialUploadDestinationState articleIds
                (fsActual, Right (Right (), udsActual)) = subject "urlPlaceholderInvalid"
            fsActual `shouldBe` initialFs
            udsActual `shouldBe` uds

        context "the URL in the matched line is filled with the URL with an articleId." $
          it "updates the exisiting article on Qiita" $ do
            let udsUpdated = M.fromList [(articleId1, Nothing), (articleId2, Just article)]
                article = Article body tags title
                body = "(Updated)\n\nTest of surrogate pair: 𠮷野家は超美味しい\nYoshino-ya is so good!\n"
                tags = [Tag "tag1" ["2.0"], Tag "tag2" ["3.1"], Tag "tag3" []]
                title = "𠮷野家に行ってきました (Updated)"
                (fsActual, Right (Right (), udsActual)) = subject "urlPlaceholderFilled"
            fsActual `shouldBe` initialFs
            udsActual `shouldBe` udsUpdated

      context "Given a markdown file NOT including a line matched the --url-placeholder option" $
          it "do nothing" $ do
            let uds = mkInitialUploadDestinationState articleIds
                (fsActual, Right (Right (), udsActual)) = subject "urlPlaceholderInvalid"
            fsActual `shouldBe` initialFs
            udsActual `shouldBe` uds
