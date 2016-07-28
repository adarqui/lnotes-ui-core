{-# LANGUAGE LambdaCase #-}

module LN.UI.Core.App (
  runCore
) where


import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.RWS.Strict
import           Data.Tuple.Select
import           Haskell.Api.Helpers

import           LN.UI.Core.Control
import           LN.UI.Core.Router
import           LN.UI.Core.State



runCore :: MonadIO m => CoreState -> m CoreState
runCore st = sel2 <$> runCoreM st (pure ())



runRoute :: MonadIO m => CoreM m ()
runRoute = do
  gets _route >>= \case
    RouteWith Home _ -> pure ()
    RouteWith _ _    -> pure ()
