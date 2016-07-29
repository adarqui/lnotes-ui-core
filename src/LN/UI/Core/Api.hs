{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitForAll #-}

module LN.UI.Core.Api (
  api
) where



import Control.Monad.Trans
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.RWS
import Control.Monad.Trans.Reader (ReaderT)
import           Haskell.Api.Helpers
import           Haskell.Api.Helpers.Shared

import           LN.UI.Core.Control
import           LN.UI.Core.State



api :: forall m a. MonadIO m => ReaderT (ApiOptions SpecificApiOptions) IO a -> CoreM m a
api actions = do
  opts <- asks _apiOptions
  liftIO $ runWith actions opts
