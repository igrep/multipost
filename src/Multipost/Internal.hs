{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Multipost.Internal
  ( Action (..)
  , splitMetadata
  , decideWhatToDoWith
  , runPreprocessors
  , replaceUrlField
  ) where

import           Control.Exception.Safe (throwString)
import           Control.Monad          (foldM)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.Result   (result)
import           Data.Bifunctor         (second)
import qualified Data.Reflection        as R
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Void              (Void)
import qualified Data.Yaml              as Y
import qualified Text.Megaparsec        as M
import qualified Text.Megaparsec.Char   as MC
import           Text.RE.Replace        (Capture (Capture, captureLength, captureOffset),
                                         capturedText, matchCaptures)
import           Text.RE.TDFA           (SimpleREOptions (MultilineSensitive),
                                         compileSearchReplaceWith,
                                         makeRegexWith, re, (*=~/), (?=~),
                                         (?=~/))
import           Text.RE.TDFA.Text      (escapeREString)

import           Multipost.Types        (Arguments, ArticleId, Metadata,
                                         Preprocessor (..))


splitMetadata :: MonadThrow m => Arguments -> FilePath -> T.Text -> m (Metadata, T.Text)
splitMetadata args path contents = do
  (metaContents, left) <-  either (throwString . M.errorBundlePretty) return $ M.runParser p path contents
  meta <- R.give args $ Y.decodeThrow . TE.encodeUtf8 $ T.pack metaContents
  return (meta, left)
 where
  p :: M.Parsec Void T.Text (String, T.Text)
  p = do
    _ <- MC.string "---"
    _ <- MC.eol
    (,)
      <$> M.manyTill M.anySingle (M.try (MC.eol *> MC.string "---" *> MC.eol))
      <*> M.takeRest


data Action =
  PostNew (Capture T.Text) | PatchExisting ArticleId
  deriving (Eq, Show)


decideWhatToDoWith :: MonadThrow m => T.Text -> T.Text -> T.Text -> m Action
decideWhatToDoWith entireContents canonicalUrlKey canonicalUrlContents =
  if canonicalUrlContents == "qiita"
    then do
      let qs = "[\"']?"
          ss = "[ \t　]*" -- NOTE: \s doesn't seem available in the regex package
          reStr =
              "^"
            ++ qs
            ++ escapeREString (T.unpack canonicalUrlKey)
            ++ qs
            ++ ":"
            ++ ss
            ++ qs
            ++ "("
            ++ escapeREString (T.unpack canonicalUrlContents)
            ++ ")"
            ++ qs
            ++ ss
            ++ "$"
      regex <- result throwString return
        $ makeRegexWith MultilineSensitive reStr
      let captureUrlPlaceholder = matchCaptures $ entireContents ?=~ regex
      case captureUrlPlaceholder of
          Just (_, [matchedCanonicalUrlContents]) ->
            return . PostNew $ matchedCanonicalUrlContents
          other ->
            throwString $ "Assertion failure: " ++ show other
    else do
      let captureArticleUrl =
            matchCaptures $ canonicalUrlContents ?=~ [re|https://qiita.com/[^/]+/items/([^/]+)|]
      case captureArticleUrl of
          Just (_url, [itemId]) ->
            return . PatchExisting $ capturedText itemId
          _other ->
            throwString $ "Invalid canonical URL: " ++ show canonicalUrlContents


runPreprocessors :: MonadThrow m => [Preprocessor] -> T.Text -> m T.Text
runPreprocessors = flip $ foldM f
 where
  f body Preprocessor { preprocessorRe, preprocessorReplacement, preprocessorReOpts, preprocessorGlobal } = do
    sr <- result throwString return $
      compileSearchReplaceWith preprocessorReOpts preprocessorRe preprocessorReplacement
    let op = if preprocessorGlobal then (*=~/) else (?=~/)
    return $ body `op` sr


replaceUrlField :: Capture T.Text -> T.Text -> T.Text -> T.Text
replaceUrlField Capture { captureOffset = off, captureLength = len } url entire =
  beforePlaceholder <> url <> afterPlaceholder
 where
  (beforePlaceholder, afterPlaceholder) = second (T.drop len) $ T.splitAt off entire
