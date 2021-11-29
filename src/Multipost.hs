{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Multipost
  ( mainWith
  , module Multipost.Types
  , module Multipost.UploadDestination.Qiita
  ) where


import           Control.Monad.Catch               (MonadThrow, throwM)
import           Data.Foldable                     (for_)
import qualified Data.Text                         as T
import           Prelude                           hiding (readFile, writeFile)

import           Multipost.Internal
import           Multipost.Types
import           Multipost.UploadDestination.Qiita


mainWith :: MonadThrow m => Env m -> Arguments -> m ()
mainWith
  Env { qiita, logDebug, readFile, writeFile }
  args@Arguments { canonicalUrlKey, targetMarkdownPaths } =
    for_ targetMarkdownPaths $ \targetMarkdownPath -> do
      entire <- readFile targetMarkdownPath
      (meta, body0) <- splitMetadata args targetMarkdownPath entire
      case metadataCanonicalUrl meta of
          Just canonicalUrl -> do
            action <- decideWhatToDoWith entire canonicalUrlKey canonicalUrl
            body1 <- runPreprocessors (metadataPreprocessors meta) body0
            let article = Article
                  { articleTitle = metadataTitle meta
                  , articleTags = metadataTags meta
                  , articleBody = T.strip body1
                  }
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
