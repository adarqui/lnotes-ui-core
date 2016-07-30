{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}

module LN.UI.Core.Control (
    CoreM
  , CoreResult (..)
  , CoreReader
  , CoreWriter
  , CoreState
  , runCoreM
  , start
  , next
  , done
  , unit
) where



import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Trans.RWS
import           Data.Tuple.Select       (sel2)
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)

import           LN.UI.Core.Router.Route
import           LN.UI.Core.State



type CoreM m = RWST CoreReader CoreWriter CoreState m

type CoreReader = ImmutableStore
type CoreWriter = ()
type CoreState  = Store



data CoreResult
  = Start
  | Next
  | Done
  | Error
  | Reroute RouteWith
  deriving (Generic, Typeable)



start :: Monad m => m CoreResult
start = pure Start



next :: Monad m => m CoreResult
next = pure Next



done :: Monad m => m CoreResult
done = pure Done



unit :: Monad m => m ()
unit = pure ()



runCoreM :: forall m. MonadIO m => CoreState -> CoreM m CoreResult -> m (CoreResult, CoreState)
runCoreM st act = do
  (result, st, _) <- runRWST act defaultImmutableStore st
  pure (result, st)
