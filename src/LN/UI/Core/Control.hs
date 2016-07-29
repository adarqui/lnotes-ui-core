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
import Data.Tuple.Select (sel2)

import           LN.UI.Core.State



type CoreM m = RWST CoreReader CoreWriter CoreState m

type CoreReader = ImmutableStore
type CoreWriter = ()
type CoreState  = Store



-- runCoreM :: forall m a. MonadIO m => CoreState -> CoreM m a -> m (a, CoreState, CoreWriter)
-- runCoreM st act = runRWST act defaultImmutableStore st

runCoreM :: forall m a. MonadIO m => CoreState -> CoreM m a -> m CoreState
runCoreM st act = sel2 <$> runRWST act defaultImmutableStore st
