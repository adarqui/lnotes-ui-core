{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards #-}

module LN.UI.Core.App (
  runCore
) where


import           Control.Monad.IO.Class     ()
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Either
import           Data.Rehtie
import           Haskell.Helpers.Either

import           LN.Api
import qualified LN.Api.String as ApiS
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
  -> (CoreResult CoreState)                -- ^ We fetch data differently based on CoreResult
  -> Action                    -- ^ The action we are operating one
  -> m (CoreResult CoreState, CoreState) -- ^ The newly computed route & state

runCore st core_result action = runCoreM st $ do
  case action of
    Init             -> basedOn load_init fetch_init
    Route route_with -> act_route route_with
    _ -> final

  where

  basedOn final_ refeed_ = case core_result of
    Final  -> final_
    Refeed -> refeed_
    _      -> error "bleh"



  fetch_init = do
    lr <- api getMe'
    rehtie
      lr
      (const cantLoad_init)
      $ \user_pack -> applyFinal (\st_->st_{_l_m_me = Loaded $ Just user_pack})

  load_init = modify (\st'->st'{_l_m_me = Loading}) *> refeed

  cantLoad_init = applyFinal (\st'->st'{_l_m_me = CantLoad})



  setRoute route_with = modify (\st'->st'{_route = route_with})

  act_route route_with = setRoute route_with *> case route_with of
    RouteWith Home _   -> final
    RouteWith About _  -> final
    RouteWith Portal _ -> final

    RouteWith (Organizations New) _               -> load_organizations_new
    RouteWith (Organizations Index) _             -> basedOn load_organizations_index fetch_organizations_index
    RouteWith (Organizations (ShowS org_sid)) _   -> basedOn (load_organization_show org_sid) (fetch_organization_show org_sid)
    RouteWith (Organizations (EditS org_sid)) _   -> basedOn (load_organization org_sid) (fetch_organization org_sid)
    RouteWith (Organizations (DeleteS org_sid)) _ -> basedOn (load_organization org_sid) (fetch_organization org_sid)

    RouteWith (OrganizationsForums org_sid Index) _  -> basedOn (load_organizations_forums_index org_sid) (fetch_organizations_forums_index org_sid)

    RouteWith (Users Index) _              -> basedOn load_users_index fetch_users_index
    RouteWith (Users (ShowS user_sid)) _   -> final
    RouteWith (Users (EditS user_sid)) _   -> final
    RouteWith (Users (DeleteS user_sid)) _ -> final

    RouteWith _ _ -> final

    where
    route               = case route_with of RouteWith route' _ -> route'
    params              = case route_with of RouteWith _ params' -> params'
    page_info           = pageInfoFromParams params
    params_list         = paramsFromPageInfo page_info
    new_page_info count = runPageInfo count page_info



    load_organizations_new = applyFinal (\st'->st'{_l_m_organizationRequest = Loaded (Just defaultOrganizationRequest)})

    load_organizations_index = modify (\st'->st'{_l_organizations = Loading}) *> refeed

    cantLoad_organizations_index = applyFinal (\st'->st'{_l_organizations = CantLoad})

    fetch_organizations_index = do
      lr <- runEitherT $ do
        count         <- mustPassT $ api $ getOrganizationsCount'
        organizations <- mustPassT $ api $ getOrganizationPacks params_list
        pure (count, organizations)
      rehtie lr (const $ cantLoad_organizations_index) $ \(count, organization_packs) -> do
        applyFinal (\st'->st'{
            _l_organizations = Loaded $ idmapFrom organizationPackResponseOrganizationId (organizationPackResponses organization_packs)
          , _pageInfo = new_page_info count
          })



    load_users_index = modify (\st'->st'{_l_users = Loading}) *> refeed

    cantLoad_users_index = applyFinal (\st'->st'{_l_users = CantLoad})

    fetch_users_index = do
      lr <- runEitherT $ do
        count <- mustPassT $ api $ getUsersCount'
        users <- mustPassT $ api $ getUserSanitizedPacks params_list
        pure (count, users)
      rehtie lr (const $ cantLoad_users_index) $ \(count, user_packs) -> do
        applyFinal (\st'->st'{
            _l_users = Loaded $ idmapFrom userSanitizedPackResponseUserId (userSanitizedPackResponses user_packs)
          , _pageInfo = new_page_info count
          })



    load_organization_show org_sid = final

--    cantLoad_organization_show = final

    fetch_organization_show org_sid = final



    load_organization org_sid = modify (\st'->st'{_l_m_organization = Loading}) *> refeed

    cantLoad_organization = applyFinal (\st'->st'{_l_m_organization = CantLoad})

    fetch_organization org_sid = do
      lr <- runEitherT $ do
        organization <- mustPassT $ api $ ApiS.getOrganizationPack' org_sid
        pure organization
      rehtie lr (const $ cantLoad_organization) $ \organization -> do
        applyFinal (\st'->st'{
          _l_m_organization = Loaded $ Just organization
        })



    load_forums_index _ = modify (\st'->st'{_l_forums = Loading}) *> refeed

    cantLoad_forums_index = applyCore (\st'->st'{_l_forums = CantLoad})

    fetch_forums_index org_sid = do
      lr <- runEitherT $ do
        organization <- mustPassT $ api $ ApiS.getOrganizationPack' org_sid
        let OrganizationPackResponse{..} =  organization
        forums       <- mustPassT $ api $ getForumPacks_ByOrganizationId' organizationPackResponseOrganizationId
        pure (organization, forums)
      rehtie lr (const cantLoad_forums_index) $ \(organization, forums) -> do
        applyCore (\st'->st'{
          _l_m_organization = Loaded $ Just organization
        , _l_forums         = Loaded $ idmapFrom forumPackResponseForumId (forumPackResponses forums)
        })



    load_organizations_forums_index org_sid = do
      f_org <- load_organization org_sid
      f_forums <- load_forums_index org_sid
      pure f_org

    cantLoad_organizations_forums_index = do
      f_org <- cantLoad_organization
      f_forums <- cantLoad_forums_index
      pure f_org

    fetch_organizations_forums_index org_sid = do
      f_org <- fetch_organization org_sid
      f_forums <- fetch_forums_index org_sid
      pure f_org
