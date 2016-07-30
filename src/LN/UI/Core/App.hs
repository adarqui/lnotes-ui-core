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
  -> CoreResult                -- ^ We fetch data differently based on CoreResult
  -> Action                    -- ^ The action we are operating one
  -> m (CoreResult, CoreState) -- ^ The newly computed route & state

runCore st core_result (ApplyState f) = pure (core_result, f st)
runCore st core_result (MachNext action) = runCore st Next action
runCore st core_result action = runCoreM st $ do
  case action of
    Init             -> basedOn load_init fetch_init
    Route route_with -> act_route route_with
    _ -> start

  where

  basedOn_ core_result_ start_ next_ done_ = case core_result_ of
    Start -> start_
    Next  -> next_
    Done  -> done_
    _     -> error "bleh"

  basedOn start_ next_ = basedOn_ core_result start_ next_ done



  fetch_init = do
    lr <- api getMe'
    rehtie
      lr
      (const cantLoad_init)
      $ \user_pack -> modify (\st_->st_{_l_m_me = Loaded $ Just user_pack}) *> done

  load_init = modify (\st'->st'{_l_m_me = Loading}) *> next

  cantLoad_init = modify (\st'->st'{_l_m_me = CantLoad}) *> done



  setRoute route_with = modify (\st'->st'{_route = route_with})

  act_route route_with = setRoute route_with *> case route_with of
    RouteWith Home _   -> start
    RouteWith About _  -> start
    RouteWith Portal _ -> start

    RouteWith (Organizations New) _               -> load_organizations_new *> done
    RouteWith (Organizations Index) _             -> basedOn load_organizations_index fetch_organizations_index
    RouteWith (Organizations (ShowS org_sid)) _   -> basedOn (load_organization_show org_sid) (fetch_organization_show org_sid)
    RouteWith (Organizations (EditS org_sid)) _   -> basedOn (load_organization org_sid) (fetch_organization org_sid)
    RouteWith (Organizations (DeleteS org_sid)) _ -> basedOn (load_organization org_sid) (fetch_organization org_sid)

    RouteWith (OrganizationsForums org_sid Index) _  -> basedOn (load_organizations_forums_index org_sid) (fetch_organizations_forums_index org_sid)

    RouteWith (Users Index) _              -> basedOn load_users_index fetch_users_index
    -- RouteWith (Users (ShowS user_sid)) _   -> start
    -- RouteWith (Users (EditS user_sid)) _   -> start
    -- RouteWith (Users (DeleteS user_sid)) _ -> start

    RouteWith _ _ -> start

    where
    route               = case route_with of RouteWith route' _ -> route'
    params              = case route_with of RouteWith _ params' -> params'
    page_info           = pageInfoFromParams params
    params_list         = paramsFromPageInfo page_info
    new_page_info count = runPageInfo count page_info



    load_organizations_new = modify (\st'->st'{_m_organizationRequest = Just defaultOrganizationRequest}) *> next



    load_organizations_index = modify (\st'->st'{_l_organizations = Loading}) *> next

    cantLoad_organizations_index = modify (\st'->st'{_l_organizations = CantLoad}) *> done

    fetch_organizations_index = do
      lr <- runEitherT $ do
        count         <- mustPassT $ api $ getOrganizationsCount'
        organizations <- mustPassT $ api $ getOrganizationPacks params_list
        pure (count, organizations)
      rehtie lr (const $ cantLoad_organizations_index) $ \(count, organization_packs) -> do
        modify (\st'->st'{
            _l_organizations = Loaded $ idmapFrom organizationPackResponseOrganizationId (organizationPackResponses organization_packs)
          , _pageInfo = new_page_info count
          })
        done



    load_users_index = modify (\st'->st'{_l_users = Loading}) *> next

    cantLoad_users_index = modify (\st'->st'{_l_users = CantLoad}) *> done

    fetch_users_index = do
      lr <- runEitherT $ do
        count <- mustPassT $ api $ getUsersCount'
        users <- mustPassT $ api $ getUserSanitizedPacks params_list
        pure (count, users)
      rehtie lr (const $ cantLoad_users_index) $ \(count, user_packs) -> do
        modify (\st'->st'{
            _l_users = Loaded $ idmapFrom userSanitizedPackResponseUserId (userSanitizedPackResponses user_packs)
          , _pageInfo = new_page_info count
          })
        done



    load_organization_show org_sid = done

    fetch_organization_show org_sid = done



    load_organization org_sid = modify (\st'->st'{_l_m_organization = Loading}) *> next

    cantLoad_organization = modify (\st'->st'{_l_m_organization = CantLoad}) *> done

    fetch_organization org_sid = do
      lr <- runEitherT $ do
        organization <- mustPassT $ api $ ApiS.getOrganizationPack' org_sid
        pure organization
      rehtie lr (const $ cantLoad_organization) $ \organization -> do
        modify (\st'->st'{
          _l_m_organization = Loaded $ Just organization
        })
        done



    load_forums_index _ = modify (\st'->st'{_l_forums = Loading}) *> next

    cantLoad_forums_index = modify (\st'->st'{_l_forums = CantLoad}) *> done

    fetch_forums_index org_sid = do
      lr <- runEitherT $ do
        organization <- mustPassT $ api $ ApiS.getOrganizationPack' org_sid
        let OrganizationPackResponse{..} =  organization
        forums       <- mustPassT $ api $ getForumPacks_ByOrganizationId' organizationPackResponseOrganizationId
        pure (organization, forums)
      rehtie lr (const cantLoad_forums_index) $ \(organization, forums) -> do
        modify (\st'->st'{
          _l_m_organization = Loaded $ Just organization
        , _l_forums         = Loaded $ idmapFrom forumPackResponseForumId (forumPackResponses forums)
        })
        done



    load_organizations_forums_index org_sid = do
      load_organization org_sid
      load_forums_index org_sid
      next

    cantLoad_organizations_forums_index = do
      void $ cantLoad_organization
      void $ cantLoad_forums_index
      done

    fetch_organizations_forums_index org_sid = do
      Store{..} <- get
      case (_l_m_organization, _l_forums) of
        (Loading, _) -> fetch_organization org_sid >>= \core_result_ -> basedOn_ core_result_ start next done
        (Loaded _, Loading) -> fetch_forums_index org_sid >>= \core_result_ -> basedOn_ core_result_ start next done
        _ -> done
      done
