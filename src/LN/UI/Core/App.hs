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
  -> Action                    -- ^ The action we are operating on
  -> m (CoreResult, CoreState) -- ^ The newly computed route & state

runCore st core_result (ApplyState f) = pure (core_result, f st)
runCore st _ (MachNext action)        = runCore st Next action
runCore st core_result action         = runCoreM st $ do
  case action of
    Init             -> basedOn load_init fetch_init
    Route route_with -> act_route route_with
    Save             -> act_save

    -- Operations that should only run on a frontend.
    _ -> done

  where

  basedOn_ core_result_ start_ next_ done_ = case core_result_ of
    Start     -> start_
    Next      -> next_
    Done      -> done_
    Failure   -> failure
    Reroute r -> reroute r

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
    RouteWith (Organizations (ShowS org_sid)) _   -> basedOn load_organization_show (fetch_organization_show org_sid)
    RouteWith (Organizations (EditS org_sid)) _   -> basedOn load_organization (fetch_organization org_sid)
    RouteWith (Organizations (DeleteS org_sid)) _ -> basedOn load_organization (fetch_organization org_sid)

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
      rehtie lr (const cantLoad_users_index) $ \(count, user_packs) -> do
        modify (\st'->st'{
            _l_users = Loaded $ idmapFrom userSanitizedPackResponseUserId (userSanitizedPackResponses user_packs)
          , _pageInfo = new_page_info count
          })
        done



    load_organization_show = do
      void load_organization
      void load_forums_index
      next

    fetch_organization_show org_sid = do
      fetch_organizations_forums_index org_sid



    load_organization = modify (\st'->st'{_l_m_organization = Loading}) *> next

    cantLoad_organization = modify (\st'->st'{_l_m_organization = CantLoad}) *> done

    fetch_organization org_sid = do
      lr <- api $ ApiS.getOrganizationPack' org_sid
      rehtie lr (const cantLoad_organization) $ \organization -> do
        modify (\st'->st'{
          _l_m_organization = Loaded $ Just organization
        })
        done



    load_forums_index = modify (\st'->st'{_l_forums = Loading}) *> next

    cantLoad_forums_index = modify (\st'->st'{_l_forums = CantLoad}) *> done

    fetch_forums_index org_sid = do
      Store{..} <- get
      case _l_m_organization of
        Loaded (Just organization@OrganizationPackResponse{..}) -> do
          lr <- api $ getForumPacks_ByOrganizationId' organizationPackResponseOrganizationId
          rehtie lr (const cantLoad_forums_index) $ \ForumPackResponses{..} -> do
            modify (\st'->st'{
              _l_forums = Loaded $ idmapFrom forumPackResponseForumId forumPackResponses
            })
            done
        _ -> cantLoad_forums_index



    load_organizations_forums_index org_sid = do
      void load_organization
      void load_forums_index
      next

    cantLoad_organizations_forums_index = do
      void cantLoad_organization
      void cantLoad_forums_index
      done

    fetch_organizations_forums_index org_sid = do
      Store{..} <- get
      case (_l_m_organization, _l_forums) of
        (Loading, _)         -> fetch_organization org_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded _, Loading)  -> fetch_forums_index org_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded _, Loaded _) -> done
        _                    -> cantLoad_organizations_forums_index




  act_save = do
    route_with <- gets _route
    case route_with of
      RouteWith (Organizations _) _ -> act_save_organization
      RouteWith (Users _) _         -> done
      RouteWith (OrganizationsForums _ _) _ -> done
      _ -> done

    where
    act_save_organization = do
      Store{..} <- get
      case (_l_m_me, _m_organizationRequest) of
        (Loaded (Just UserResponse{..}), Just request) -> do
          lr <- api $ postOrganization' (request { organizationRequestEmail = userResponseEmail })
          rehtie lr (const done) $ \OrganizationResponse{..} -> do
            reroute (RouteWith (Organizations (ShowS organizationResponseName)) emptyParams)
        _ ->  done
