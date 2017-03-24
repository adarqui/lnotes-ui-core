{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | MUST REDUCE THE SIZE OF THIS FILE TO ~500 LINES
-- Too much redundant insanity.

module LN.UI.Core.App (
  runCore
) where



import           Control.Monad.IO.Class     ()
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Either
import           Data.Int                   (Int64)
import           Data.List                  (nub)
import qualified Data.Map                   as Map
import           Data.Rehtie
import           Haskell.Api.Helpers
import           Haskell.Api.Helpers.Shared
import           Haskell.Helpers.Either

import           LN.Api
import qualified LN.Api.String              as ApiS
import           LN.Generate.Default
import           LN.T
import           LN.T.Convert
import           LN.T.Param
import           LN.UI.Core.Api
import           LN.UI.Core.Control
import           LN.UI.Core.Helpers.Map
import           LN.UI.Core.Loader
import           LN.UI.Core.PageInfo
import           LN.UI.Core.Router
import           LN.UI.Core.State
import           LN.UI.Core.Types



-- | Our state machine
--
runCore
  :: forall m. MonadIO m
  => CoreState                 -- ^ Our current State
  -> CoreResult                -- ^ We fetch data differently based on CoreResult
  -> Action                    -- ^ The action we are operating on
  -> m (CoreResult, CoreState) -- ^ The newly computed route & state

runCore st core_result (ApplyStateRender b f) = pure (core_result, (f st) { _render = b })
runCore st core_result (ApplyState f) = pure (core_result, f st)
runCore st _ (MachNext action)        = runCore st Next action
runCore st core_result action         = runCoreM st $ do
  case action of
    Init                     -> basedOn load_init fetch_init
    Route route_with         -> act_route route_with
    MergeUsers users         -> act_merge_users users
    MergeUserIds ids         -> act_merge_user_ids ids
    Save                     -> act_save

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
      $ \user_pack -> do
        let UserResponse{..} = user_pack
        modify (\st_->st_{_l_m_me = Loaded $ Just user_pack, _meId = userResponseId})
        done

  load_init = modify (\st'->st'{_l_m_me = Loading}) *> next

  cantLoad_init = modify (\st'->st'{_l_m_me = CantLoad}) *> done



  setRoute route_with = modify (\st'->st'{_route = route_with})

  act_route route_with = setRoute route_with *> case route_with of
    RouteWith Home _   -> start
    RouteWith About _  -> start
    RouteWith Portal _ -> start

    RouteWith (Users Index) _                 -> basedOn load_users fetch_users
    RouteWith (Users (ShowS user_sid)) _      -> basedOn load_user (fetch_user user_sid)
    RouteWith (UsersProfile user_sid _) _     -> basedOn load_user (fetch_user user_sid)

    RouteWith (Experiments _) _ -> do_experiments

    RouteWith _ _               -> start

    where
    route               = case route_with of RouteWith route' _ -> route'
    params              = case route_with of RouteWith _ params' -> params'
    page_info           = pageInfoFromParams params
    params_list         = paramsFromPageInfo page_info
    params_list_dsc     = paramsFromPageInfo (page_info { sortOrder = SortOrderBy_Dsc, order = OrderBy_ActivityAt })

    new_page_info count = runPageInfo count page_info




    do_experiments :: MonadIO m => CoreM m CoreResult
    do_experiments = do
      -- Just load up some mock stuff that we may need for experiments
      modify (\st'->st'{
        _m_threadPostRequest = Just defaultThreadPostRequest
      })
      done


    load_users :: MonadIO m => CoreM m CoreResult
    load_users = modify (\st'->st'{_l_users = Loading}) *> next

    cantLoad_users :: MonadIO m => CoreM m CoreResult
    cantLoad_users = modify (\st'->st'{_l_users = CantLoad}) *> done

    fetch_users :: MonadIO m => CoreM m CoreResult
    fetch_users = do
      lr <- runEitherT $ do
        count <- mustPassT $ api $ getUsersCount'
        users <- mustPassT $ api $ getUserSanitizedPacks params_list
        pure (count, users)
      rehtie lr (const cantLoad_users) $ \(count, user_packs) -> do
        modify (\st'->st'{
            _l_users = Loaded $ idmapFrom userSanitizedPackResponseUserId (userSanitizedPackResponses user_packs)
          , _pageInfo = new_page_info count
          })
        done



    load_user :: MonadIO m => CoreM m CoreResult
    load_user = modify (\st'->st'{_l_m_user = Loading}) *> next

    cantLoad_user :: MonadIO m => CoreM m CoreResult
    cantLoad_user = modify (\st'->st'{_l_m_user = CantLoad}) *> done

    fetch_user :: MonadIO m => UserName -> CoreM m CoreResult
    fetch_user user_sid = do
      lr <- api $ ApiS.getUserSanitizedPack' user_sid
      rehtie lr (const cantLoad_user) $ \user@UserSanitizedPackResponse{..} -> do
        modify (\st'->st'{
            _l_m_user = Loaded $ Just user
          , _m_profileRequest = Just $ profileResponseToProfileRequest [] Nothing userSanitizedPackResponseProfile
        })
        done



    -- load_users_profile :: MonadIO m => CoreM m CoreResult
    -- load_users_profile = modify (\st'->st'{_l_m_profile = Loading}) *> next

    -- cantLoad_users_profile :: MonadIO m => CoreM m CoreResult
    -- cantLoad_users_profile = modify (\st'->st'{_l_m_profile = CantLoad}) *> done

    -- fetch_users_profile :: MonadIO m => UserName -> CoreM m CoreResult
    -- fetch_users_profile user_sid = do
    --   lr <- runEitherT $ do
    --     pure (count, profiles)
    --   rehtie lr (const $ cantLoad_users_profile) $ \(count, profile_packs) -> do
    --     modify (\st'->st'{
    --         _l_m_profile = Loaded $ idmapFrom profilePackResponseOrganizationId (profilePackResponses profile_packs)
    --       , _pageInfo = new_page_info count
    --       })
    --     done



    -- load_users_profile_index :: MonadIO m => CoreM m CoreResult
    -- load_users_profile_index = do
    --   load_user
    --   next

    -- fetch_users_profile_index :: MonadIO m => UserName -> CoreM m CoreResult
    -- fetch_users_profile_index user_sid = do






  act_save = do
    route_with <- gets _route
    case route_with of
      RouteWith (UsersProfile _ _) _                                -> act_save_users_profile
      RouteWith (Users _) _                                         -> done
      _ -> done

    where
    act_save_users_profile :: MonadIO m => CoreM m CoreResult
    act_save_users_profile = do
      Store{..} <- get
      case (_l_m_user, _m_profileRequest) of
        (Loaded (Just UserSanitizedPackResponse{..}), Just request) -> do
          let
            UserSanitizedResponse{..} = userSanitizedPackResponseUser
            ProfileResponse{..}       = userSanitizedPackResponseProfile

          lr <- api $ putUserProfile' profileResponseId request
          rehtie lr (const done) $ \_ -> do
            reroute $ RouteWith (UsersProfile userSanitizedResponseName Index) emptyParams

        _ ->  done



  -- | Takes a list of sanitized users, and pulls down any of them that
  -- don't already exist in the current usersCache
  --
  act_merge_users users = act_merge_user_ids $ map userSanitizedResponseId users



  -- | Takes a list of user ids, and pulls down any of them that
  -- don't already exist in the current _usersCache
  --
  act_merge_user_ids user_ids = do
    Store{..} <- get
    let
      user_ids_not_in_map =
        filter (\user_id -> not $ Map.member user_id _usersCache)
        $ nub user_ids

    case user_ids_not_in_map of
      [] -> done
      xs -> do
        lr <- api $ getUserSanitizedPacks_ByUsersIds' user_ids_not_in_map
        rehtie lr (const done) $ \UserSanitizedPackResponses{..} -> do
          let new_users_map = idmapFrom userSanitizedPackResponseUserId userSanitizedPackResponses
          Store{..} <- get
          modify (\st'->st'{_usersCache = Map.union new_users_map _usersCache})
          done

  -- unexpectedError = errorMessage "Unexpected error."

  apiError (DecodeError _)   = errorMessage "Unexpected JSON decode error."
  apiError (ServerError _ _) = errorMessage "Unexpected server error."

  errorMessage msg = setError msg

  setError msg = do
    modify (\st'@Store{..}->st'{ _errors = msg : _errors })
    done

--   clearErrors = do
--     modify (\st'->st'{ _errorText = [] })
--     done





-- TODO FIXME
-- move this somewhere else, ie, ln-api
--
postLike :: Ent -> Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
postLike ent ent_id like_request =
  case ent of
    Ent_ThreadPost -> postLike_ByThreadPostId' ent_id like_request
    _              -> error "unsupported"



-- TODO FIXME
-- move this somewhere else, ie, ln-api
--
postStar :: Ent -> Int64 -> StarRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) StarResponse)
postStar ent ent_id star_request =
  case ent of
    Ent_ThreadPost -> postStar_ByThreadPostId' ent_id star_request
    _              -> error "unsupported"
