{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Multipost.Internal
  ( Action (..)
  , decideWhatToDoWith
  , extractTitle
  , extractTags
  , dropMetadata
  , replaceUrlField
  ) where

import           Control.Exception.Safe (throwString)
import           Control.Monad.Catch    (MonadThrow)
import           Data.Bifunctor         (second)
import           Data.Either.Result     (result, toEither)
import qualified Data.Text              as T
import           Data.Traversable       (for)
import           Text.RE.Replace        (Capture (Capture, captureLength, captureOffset),
                                         capturedText, matchCaptures)
import           Text.RE.TDFA           (SimpleREOptions (BlockSensitive, MultilineSensitive),
                                         makeRegexWith, makeSearchReplaceWith,
                                         re, (?=~), (?=~/))

import           Multipost.Types

import           Debug.Trace


data Action =
  PostNew (Capture T.Text) | PatchExisting ArticleId
  deriving (Eq, Show)


decideWhatToDoWith :: MonadThrow m => UserRegex -> T.Text -> m (Maybe Action)
decideWhatToDoWith urlPlaceholderReStr entire = do
  urlPlaceholderRe <- result throwString return $ makeRegexWith MultilineSensitive urlPlaceholderReStr
  let mCaptureUrlPlaceholder = matchCaptures $ entire ?=~ urlPlaceholderRe
  for mCaptureUrlPlaceholder $ \(_allMatch, captures) ->
    case captures of
        [theOnlyCapture] -> do
          let maybeUrl = T.strip $ capturedText theOnlyCapture
          if T.null maybeUrl || maybeUrl == "qiita"
            then return $ PostNew theOnlyCapture
            else do
              let captureArticleUrl =
                    matchCaptures $ traceShowId maybeUrl ?=~ [re|https://qiita.com/[^/]+/items/([^/]+)|]
              case traceShowId captureArticleUrl of
                  Just (_url, [itemId]) ->
                    return . PatchExisting $ capturedText itemId
                  Just (_, other) ->
                    throwString
                      $ "Assertion failure: capturing more than one itemId or none: "
                      ++ show other
                  Nothing ->
                    throwString
                      $ "Unexpected URL placeholder value extracted: "
                      ++ show maybeUrl ++ " is neither just \"qiita.com\" nor a URL of an item on qiita!"
        _ ->
          throwString
            $ "The --url-placeholder option must capture *only one* URL of the article. "
            ++ "But \"" ++ T.unpack urlPlaceholderReStr ++ "\" doesn't!"


extractTitle :: MonadThrow m => FilePath -> UserRegex -> T.Text -> m T.Text
extractTitle path titleReStr entire = do
  titleRe <- result throwString return $ makeRegexWith MultilineSensitive titleReStr
  let mCaptureTitle = matchCaptures $ entire ?=~ titleRe
  case mCaptureTitle of
      Just (_allMatch, [theOnlyCapture]) ->
        return . T.strip $ capturedText theOnlyCapture
      Just (_allMatch, _other) ->
        throwString
          $ "The --title option must capture *only one* title of " ++ path ++ ". "
          ++ "But \"" ++ T.unpack titleReStr ++ "\" doesn't!"
      Nothing ->
        throwString
          $ "The --title option didn't match with the content of " ++ path ++ "!"


extractTags :: MonadThrow m => FilePath -> UserRegex -> T.Text -> m [Tag]
extractTags path tagsReStr entire = do
  tagsRe <- result throwString return $ makeRegexWith MultilineSensitive tagsReStr
  let mCaptureTags = matchCaptures $ entire ?=~ tagsRe
  case mCaptureTags of
      Just (_allMatch, [theOnlyCapture]) ->
        return . parseQiitaTags . T.strip $ capturedText theOnlyCapture
      Just (_allMatch, _other) ->
        throwString
          $ "The --tags option must capture *only one* tags of " ++ path ++ ". "
          ++ "But \"" ++ T.unpack tagsReStr ++ "\" doesn't!"
      Nothing ->
        throwString
          $ "The --tags option didn't match with the content of " ++ path ++ "!"


parseQiitaTags :: T.Text -> [Tag]
parseQiitaTags =
  map
    ( uncurry Tag
      . second
        ( filter (not . T.null)
          . T.split (== ',')
          . T.drop 1 -- Drop the colon
        )
      . T.break (== ':'))
    . T.words


dropMetadata :: MonadThrow m => UserRegex -> T.Text -> m T.Text
dropMetadata metadataReStr entire = do
  let eMetadataSR = makeSearchReplaceWith BlockSensitive metadataReStr ""
  case toEither eMetadataSR of
      Right metadataSR -> return . T.strip $ entire ?=~/ metadataSR
      Left emsg        -> throwString emsg


replaceUrlField :: Capture T.Text -> T.Text -> T.Text -> T.Text
replaceUrlField Capture { captureOffset = off, captureLength = len } url entire =
  beforePlaceholder <> " " <> url <> afterPlaceholder
 where
  (beforePlaceholder, afterPlaceholder) = second (T.drop len) $ T.splitAt off entire
