{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase     #-}

module LN.UI.Core.App (
  runCore
) where


import           Control.Monad.IO.Class   ()
import           Control.Monad.RWS.Strict
import           Data.Rehtie

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
  -> m CoreState  -- ^ The newly computed route & state

runCore st action = runCoreM st $ do
  case action of
    Init             -> do
      act_init
      route_with <- gets _route
      act_route route_with
    Route route_with -> act_route route_with
    _ -> unit

  where

  act_init = do
    lr <- api getMe'
    rehtie
      lr
      (const unit)
      $ \user_pack -> modify (\st_->st_{_m_me = Just user_pack})

  act_route route_with = case route_with of
    RouteWith Home _ -> unit
    RouteWith (Organizations New) _ -> unit
    RouteWith (Organizations Index) params -> unit
    RouteWith (Organizations (ShowS org_sid)) _ -> unit
    RouteWith (Organizations (EditS org_sid)) _ -> unit
    RouteWith (Organizations (DeleteS org_sid)) _ -> unit
    RouteWith _ _ -> unit



unit :: Monad m => m ()
unit = pure ()
