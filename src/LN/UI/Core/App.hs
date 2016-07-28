module LN.UI.Core.App (
  runCore
) where


import           Control.Monad.IO.Class (MonadIO)
import           Data.Tuple.Select

import           LN.UI.Core.Control



runCore :: MonadIO m => CoreState -> m CoreState
runCore st = sel2 <$> runCoreM st (pure ())
