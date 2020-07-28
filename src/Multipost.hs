{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Multipost
  ( mainWith
  , module Multipost.Types
  , module Multipost.UploadDestination.Qiita
  ) where


import           Control.Monad.Catch
import           Prelude                           hiding (readFile, writeFile)

import           Multipost.Types
import           Multipost.UploadDestination.Qiita


mainWith :: MonadThrow m => Env m -> Arguments -> m ()
mainWith Env { qiita, logDebug, readFile, writeFile } Arguments { urlPlaceholderPattern, titlePattern, tagsPattern, metadataPattern } =
  undefined
