module Multipost
  ( mainWith
  , module Multipost.Types
  , module Multipost.UploadDestination.Qiita
  ) where


import           Multipost.Types
import           Multipost.UploadDestination.Qiita


mainWith :: Monad m => Env m -> [String] -> m ()
mainWith _ _ =
  error "mainWith is not defined yet!"
