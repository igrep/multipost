module Multipost
  ( mainWith
  , module Multipost.Types
  , module Multipost.UploadDestination.Qiita
  ) where

import           Control.Monad.Catch

import           Multipost.Types
import           Multipost.UploadDestination.Qiita


mainWith :: MonadThrow m => Env m -> [String] -> m ()
mainWith _ _ =
  error "mainWith is not defined yet!"
