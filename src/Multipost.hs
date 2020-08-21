{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Multipost
  ( mainWith
  , module Multipost.Types
  , module Multipost.UploadDestination.Qiita
  ) where


import           Control.Monad.Catch               (MonadThrow, throwM)
import           Data.Foldable                     (for_)
import           Prelude                           hiding (readFile, writeFile)

import           Multipost.Internal
import           Multipost.Types
import           Multipost.UploadDestination.Qiita


mainWith :: MonadThrow m => Env m -> Arguments -> m ()
mainWith
  Env { qiita, logDebug, readFile, writeFile }
  Arguments { urlPlaceholderPattern, titlePattern, tagsPattern, metadataPattern, targetMarkdownPaths } =
    for_ targetMarkdownPaths $ \targetMarkdownPath -> do
      entire <- readFile targetMarkdownPath
      mAction <- decideWhatToDoWith urlPlaceholderPattern entire
      case mAction of
          Just action -> do
            articleTitle <- extractTitle targetMarkdownPath titlePattern entire
            articleTags <- extractTags targetMarkdownPath tagsPattern entire
            articleBody <- dropMetadata metadataPattern entire
            let article = Article { articleTitle, articleTags, articleBody }
            case action of
                PostNew capture -> do
                  url <- either throwM (return . itemResponseUrl) =<< postArticle qiita article
                  writeFile targetMarkdownPath $ replaceUrlField capture url entire
                  logDebug $ "Finished publishing " ++ targetMarkdownPath ++ " on Qiita."
                PatchExisting articleId -> do
                  either throwM return =<< patchArticle qiita articleId article
                  logDebug $ "Finished updating " ++ targetMarkdownPath ++ " on Qiita."
          Nothing ->
            logDebug $ targetMarkdownPath ++ " doesn't contain URL placeholder. Ignored."
