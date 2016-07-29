{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase     #-}

module LN.UI.Core.App (
  runCore
) where


import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.RWS.Strict
import           Data.Rehtie
import           Data.Tuple.Select
import           Haskell.Api.Helpers

import           LN.Api
import           LN.UI.Core.Api
import           LN.UI.Core.Control
import           LN.UI.Core.Router
import           LN.UI.Core.State



-- | Our state machine
--
runCore
  :: forall m. MonadIO m
  => CoreState    -- ^ Our current State
  -> Action       -- ^ The action we are operating one
  -> RouteWith    -- ^ The route we are operating under
  -> m CoreState  -- ^ The newly computed route & state

runCore st action route_with = runCoreM st $ do
  case action of
    Init             -> do
      act_init
      route_with <- gets _route
      act_route route_with
    Route route_with -> act_route route_with

  where

  act_init = do
    lr <- api getMe'
    rehtie
      lr
      (const unit)
      $ \user_pack -> modify (\st->st{_m_me = Just user_pack})

  act_route route_with = case route_with of
    RouteWith Home _ -> unit
    RouteWith (Organizations New) _ -> unit
    RouteWith (Organizations Index) params -> unit
    RouteWith (Organizations (ShowS org_sid)) _ -> unit
    RouteWith (Organizations (EditS org_sid)) _ -> unit
    RouteWith (Organizations (DeleteS org_sid)) _ -> unit



runRoute :: MonadIO m => CoreM m ()
runRoute = do
  liftIO $ print "runRoute"
  gets _route >>= \case
    RouteWith Home _ -> pure ()
    RouteWith _ _    -> pure ()



unit :: Monad m => m ()
unit = pure ()
