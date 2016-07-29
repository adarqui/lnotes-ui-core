{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module LN.UI.Core.Control (
    CoreM
  , CoreResult (..)
  , CoreReader
  , CoreWriter
  , CoreState
  , runCoreM
  , refeed
  , final
  , apply
  , applyRefeed
  , applyFinal
  , unit
) where



import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Trans.RWS
import Data.Tuple.Select (sel2)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import           LN.UI.Core.State



type CoreM m = RWST CoreReader CoreWriter CoreState m

type CoreReader = ImmutableStore
type CoreWriter = ()
type CoreState  = Store



data CoreResult a
  = Final
  | Refeed
  | Apply (a -> a)
  | ApplyRefeed (a -> a)
  | ApplyFinal (a -> a)
  deriving (Generic, Typeable)


refeed :: Monad m => m (CoreResult a)
refeed = pure Refeed



final :: Monad m => m (CoreResult a)
final = pure Final



apply :: Monad m => (a -> a) -> m (CoreResult a)
apply a = pure (Apply a)



applyRefeed :: Monad m => (a -> a) -> m (CoreResult a)
applyRefeed a = pure (ApplyRefeed a)



applyFinal :: Monad m => (a -> a) -> m (CoreResult a)
applyFinal a = pure (ApplyFinal a)



unit :: Monad m => m ()
unit = pure ()



runCoreM :: forall m. MonadIO m => CoreState -> CoreM m (CoreResult CoreState) -> m (CoreResult CoreState, CoreState)
runCoreM st act = do
  (result, st, _) <- runRWST act defaultImmutableStore st
  pure (result, st)
