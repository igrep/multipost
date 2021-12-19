{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Multipost
  ( mainWith
  , module Multipost.Types
  , module Multipost.UploadDestination.Qiita
  ) where


import           Control.Monad                     (when)
import           Control.Monad.Catch               (MonadThrow, throwM)
import           Data.Foldable                     (for_)
import qualified Data.Text                         as T
import qualified EnvParse.Applicative              as EA
import           Prelude                           hiding (readFile, writeFile)
import           System.Exit.MonadThrow            (exitSuccess)

import           Multipost.Config
import           Multipost.Internal
import           Multipost.Types
import           Multipost.UploadDestination.Qiita


mainWith :: MonadThrow m => Env m -> Arguments -> m ()
mainWith
  Env
    { qiitaActions = QiitaActions { postArticle, patchArticle }
    , logDebug
    , readFile
    , writeFile
    , loadDotEnv
    , decodeEnv
    , puts
    }
  Arguments { printExampleDotEnv, dotEnvFiles, targetMarkdownPaths } = do
    when printExampleDotEnv $ do
      puts $ EA.generateDotEnvTemplate toEnvVarName codec
      exitSuccess

    for_ dotEnvFiles loadDotEnv
    cfg@Config
      { qiitaAccessToken
      -- , zennRepository
      , canonicalUrlKey
      } <- decodeEnv toEnvVarName codec

    for_ targetMarkdownPaths $ \targetMarkdownPath -> do
      entire <- readFile targetMarkdownPath
      (meta, body0) <- splitMetadata cfg targetMarkdownPath entire
      case metadataCanonicalUrl meta of
          Just canonicalUrl -> do
            action <- decideWhatToDoWith entire canonicalUrlKey canonicalUrl
            body1 <- runPreprocessors (metadataPreprocessors meta) body0
            let article = QiitaArticle
                  { qiitaArticleTitle = metadataTitle meta
                  , qiitaArticleTags = metadataQiitaTags meta
                  , qiitaArticleBody = T.strip body1
                  }
            case action of
                PostNew capture -> do
                  url <- either throwM (return . itemResponseUrl) =<< postArticle qiitaAccessToken article
                  writeFile targetMarkdownPath $ replaceUrlField capture url entire
                  logDebug $ "Finished publishing " ++ targetMarkdownPath ++ " on Qiita."
                PatchExisting articleId -> do
                  either throwM return =<< patchArticle qiitaAccessToken articleId article
                  logDebug $ "Finished updating " ++ targetMarkdownPath ++ " on Qiita."
          Nothing ->
            logDebug $ targetMarkdownPath ++ " doesn't contain URL placeholder. Ignored."
