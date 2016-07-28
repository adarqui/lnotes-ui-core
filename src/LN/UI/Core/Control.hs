{-# LANGUAGE RankNTypes #-}

module LN.UI.Core.Control (
    CoreM
  , CoreReader
  , CoreWriter
  , CoreState
  , runCoreM
) where



import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Trans.RWS

import           LN.UI.Core.State



type CoreM m = RWST CoreReader CoreWriter CoreState m

type CoreReader = ()
type CoreWriter = ()
type CoreState  = Store



runCoreM :: forall m a. MonadIO m => CoreState -> CoreM m a -> m (a, CoreState, CoreWriter)
runCoreM st act = runRWST act () st
