{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module LN.UI.Core.App (
  runCore
) where


import           Control.Monad.IO.Class     ()
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Either
import           Data.Rehtie
import           Haskell.Helpers.Either

import           LN.Api
import           LN.Generate.Default
import           LN.T
import           LN.UI.Core.Api
import           LN.UI.Core.Control
import           LN.UI.Core.Helpers.Map
import           LN.UI.Core.Loader
import           LN.UI.Core.PageInfo
import           LN.UI.Core.Router
import           LN.UI.Core.State



-- | Our state machine
--
runCore
  :: forall m. MonadIO m
  => CoreState                 -- ^ Our current State
  -> CoreResult                -- ^ We fetch data differently based on CoreResult
  -> Action                    -- ^ The action we are operating one
  -> m (CoreResult, CoreState) -- ^ The newly computed route & state

runCore st core_result action = runCoreM st $ do
  case action of
    Init             -> do
      act_init
      route_with <- gets _route
      act_route route_with
    Route route_with -> act_route route_with
    _ -> final

  where

  act_init = do
    lr <- api getMe'
    rehtie
      lr
      (const unit)
      $ \user_pack -> modify (\st_->st_{_m_me = Just user_pack})

  act_route route_with = case route_with of
    RouteWith Home _ -> final
    RouteWith (Organizations New) _ -> load_organizations_new
    RouteWith (Organizations Index) _ -> basedOn load_organizations_index fetch_organizations_index
    RouteWith (Organizations (ShowS org_sid)) _ -> final
    RouteWith (Organizations (EditS org_sid)) _ -> final
    RouteWith (Organizations (DeleteS org_sid)) _ -> final
    RouteWith _ _ -> final

    where
    route               = case route_with of RouteWith route' _ -> route'
    params              = case route_with of RouteWith _ params' -> params'
    page_info           = pageInfoFromParams params
    params_list         = paramsFromPageInfo page_info
    new_page_info count = runPageInfo count page_info

    basedOn final_ refeed_ = case core_result of
      Final  -> final_
      Refeed -> refeed_

    load_organizations_new = modify (\st'->st'{_l_m_organizationRequest = Loaded (Just defaultOrganizationRequest)}) *> final

    load_organizations_index = modify (\st'->st'{_l_organizations = Loading}) *> refeed

    cantLoad_organizations_index = modify (\st'->st'{_l_organizations = CantLoad}) *> final

    fetch_organizations_index = do
      lr <- runEitherT $ do
        count         <- mustPassT $ api $ getOrganizationsCount'
        organizations <- mustPassT $ api $ getOrganizationPacks params_list
        pure (count, organizations)
      rehtie lr (const $ cantLoad_organizations_index) $ \(count, organization_packs) -> do
        modify(\st'->st'{
            _l_organizations = Loaded $ idmapFrom organizationPackResponseOrganizationId (organizationPackResponses
organization_packs)
          , _pageInfo = new_page_info count
          })
        final
