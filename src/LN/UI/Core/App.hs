{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module LN.UI.Core.App (
  runCore
) where



import           Control.Monad.IO.Class     ()
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Either
import           Data.Rehtie
import           Haskell.Helpers.Either

import           LN.Api
import qualified LN.Api.String              as ApiS
import           LN.Generate.Default
import           LN.T
import           LN.T.Convert
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

    RouteWith (OrganizationsForums org_sid New) _                 -> basedOn load_organizations_forums_new (fetch_organizations_forums_new org_sid)
    RouteWith (OrganizationsForums org_sid Index) _               -> basedOn load_organizations_forums_index (fetch_organizations_forums_index org_sid)
    RouteWith (OrganizationsForums org_sid (ShowS forum_sid)) _   -> basedOn load_organizations_forums_show (fetch_organizations_forums_show org_sid forum_sid)
    RouteWith (OrganizationsForums org_sid (EditS forum_sid)) _   -> basedOn load_organizations_forums (fetch_organizations_forums org_sid forum_sid)
    RouteWith (OrganizationsForums org_sid (DeleteS forum_sid)) _ -> basedOn load_organizations_forums (fetch_organizations_forums org_sid forum_sid)

    RouteWith (OrganizationsForumsBoards org_sid forum_sid New) _                 -> basedOn load_organizations_forums_boards_new (fetch_organizations_forums_boards_new org_sid forum_sid)
    RouteWith (OrganizationsForumsBoards org_sid forum_sid Index) _               -> basedOn load_organizations_forums_boards_index (fetch_organizations_forums_boards_index org_sid forum_sid)
    RouteWith (OrganizationsForumsBoards org_sid forum_sid (ShowS board_sid)) _   -> basedOn load_organizations_forums_boards_show (fetch_organizations_forums_boards_show org_sid forum_sid board_sid)
    RouteWith (OrganizationsForumsBoards org_sid forum_sid (EditS board_sid)) _   -> basedOn load_organizations_forums_boards (fetch_organizations_forums_boards org_sid forum_sid board_sid)
    RouteWith (OrganizationsForumsBoards org_sid forum_sid (DeleteS board_sid)) _ -> basedOn load_organizations_forums_boards (fetch_organizations_forums_boards org_sid forum_sid board_sid)

    RouteWith (OrganizationsForumsBoardsThreads org_sid forum_sid board_sid New) _                  -> basedOn load_organizations_forums_boards_threads_new (fetch_organizations_forums_boards_threads_new org_sid forum_sid board_sid)
    RouteWith (OrganizationsForumsBoardsThreads org_sid forum_sid board_sid Index) _                -> basedOn load_organizations_forums_boards_threads_index (fetch_organizations_forums_boards_threads_index org_sid forum_sid board_sid)
    -- RouteWith (OrganizationsForumsBoardsThreads org_sid forum_sid board_sid (ShowS thread_sid)) _   -> basedOn load_organizations_forums_boards_threads_show (fetch_organizations_forums_boards_show org_sid forum_sid board_sid thread_sid)
    -- RouteWith (OrganizationsForumsBoardsThreads org_sid forum_sid board_sid (EditS thread_sid)) _   -> basedOn load_organizations_forums_boards_threads (fetch_organizations_forums_boards_threads org_sid forum_sid board_sid thread_sid)
    -- RouteWith (OrganizationsForumsBoardsThreads org_sid forum_sid board_sid (DeleteS thread_sid)) _ -> basedOn load_organizations_forums_boards_threads (fetch_organizations_forums_boards_threads org_sid forum_sid board_sid thread_sid)

    RouteWith (Users Index) _              -> basedOn load_users fetch_users
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





    load_organization :: MonadIO m => CoreM m CoreResult
    load_organization = modify (\st'->st'{_l_m_organization = Loading}) *> next

    cantLoad_organization :: MonadIO m => CoreM m CoreResult
    cantLoad_organization = modify (\st'->st'{_l_m_organization = CantLoad}) *> done

    fetch_organization :: MonadIO m => OrganizationName -> CoreM m CoreResult
    fetch_organization org_sid = do
      lr <- api $ ApiS.getOrganizationPack' org_sid
      rehtie lr (const cantLoad_organization) $ \organization@OrganizationPackResponse{..} -> do
        modify (\st'->st'{
            _l_m_organization = Loaded $ Just organization
          , _m_organizationRequest =
            Just $ organizationResponseToOrganizationRequest
                     Nothing -- unused state
                     organizationPackResponseOrganization
        })
        done



    load_forums_new :: MonadIO m => CoreM m CoreResult
    load_forums_new = modify (\st'->st'{_l_m_forum = Loaded Nothing, _m_forumRequest = Just defaultForumRequest}) *> next

    cantLoad_forums_new :: MonadIO m => CoreM m CoreResult
    cantLoad_forums_new = modify (\st'->st'{_m_forumRequest = Nothing}) *> done



    load_forums :: MonadIO m => CoreM m CoreResult
    load_forums = modify (\st'->st'{_l_forums = Loading}) *> next

    cantLoad_forums :: MonadIO m => CoreM m CoreResult
    cantLoad_forums = modify (\st'->st'{_l_forums = CantLoad}) *> done

    fetch_forums :: MonadIO m => CoreM m CoreResult
    fetch_forums = do
      Store{..} <- get
      case _l_m_organization of
        Loaded (Just organization@OrganizationPackResponse{..}) -> do
          lr <- api $ getForumPacks_ByOrganizationId' organizationPackResponseOrganizationId
          rehtie lr (const cantLoad_forums) $ \ForumPackResponses{..} -> do
            modify (\st'->st'{
              _l_forums = Loaded $ idmapFrom forumPackResponseForumId forumPackResponses
            })
            done
        _ -> cantLoad_forums



    load_forum :: MonadIO m => CoreM m CoreResult
    load_forum = modify (\st'->st'{_l_m_forum = Loading}) *> next

    cantLoad_forum :: MonadIO m => CoreM m CoreResult
    cantLoad_forum = modify (\st'->st'{_l_m_forum = CantLoad}) *> done

    fetch_forum :: MonadIO m => ForumName -> CoreM m CoreResult
    fetch_forum forum_sid = do
      Store{..} <- get
      case _l_m_organization of
        Loaded (Just organization@OrganizationPackResponse{..}) -> do
          lr <- api $ ApiS.getForumPack_ByOrganizationId' forum_sid organizationPackResponseOrganizationId
          rehtie lr (const cantLoad_forum) $ \forum_pack -> do
            modify (\st'->st'{
              _l_m_forum = Loaded $ Just forum_pack
            })
            done
        _ -> cantLoad_forum





    load_boards_new :: MonadIO m => CoreM m CoreResult
    load_boards_new = modify (\st'->st'{_l_m_board = Loaded Nothing, _m_boardRequest = Just defaultBoardRequest}) *> next

    cantLoad_boards_new :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_new = modify (\st'->st'{_m_boardRequest = Nothing}) *> done



    load_boards :: MonadIO m => CoreM m CoreResult
    load_boards = modify (\st'->st'{_l_boards = Loading}) *> next

    cantLoad_boards :: MonadIO m => CoreM m CoreResult
    cantLoad_boards = modify (\st'->st'{_l_boards = CantLoad}) *> done

    fetch_boards :: MonadIO m => CoreM m CoreResult
    fetch_boards = do
      Store{..} <- get
      case _l_m_forum of
        Loaded (Just forum@ForumPackResponse{..}) -> do
          lr <- api $ getBoardPacks_ByForumId' forumPackResponseForumId
          rehtie lr (const cantLoad_boards) $ \BoardPackResponses{..} -> do
            modify (\st'->st'{
              _l_boards = Loaded $ idmapFrom boardPackResponseBoardId boardPackResponses
            })
            done
        _ -> cantLoad_boards



    load_board :: MonadIO m => CoreM m CoreResult
    load_board = modify (\st'->st'{_l_m_board = Loading}) *> next

    cantLoad_board :: MonadIO m => CoreM m CoreResult
    cantLoad_board = modify (\st'->st'{_l_m_board = CantLoad}) *> done

    fetch_board :: MonadIO m => BoardName -> CoreM m CoreResult
    fetch_board board_sid = do
      Store{..} <- get
      case _l_m_forum of
        Loaded (Just forum@ForumPackResponse{..}) -> do
          lr <- api $ ApiS.getBoardPack_ByForumId' board_sid forumPackResponseForumId
          rehtie lr (const cantLoad_board) $ \board_pack -> do
            modify (\st'->st'{
              _l_m_board = Loaded $ Just board_pack
            })
            done
        _ -> cantLoad_board






    load_threads_new :: MonadIO m => CoreM m CoreResult
    load_threads_new = modify (\st'->st'{_l_m_thread = Loaded Nothing, _m_threadRequest = Just defaultThreadRequest}) *> next

    cantLoad_threads_new :: MonadIO m => CoreM m CoreResult
    cantLoad_threads_new = modify (\st'->st'{_m_threadRequest = Nothing}) *> done



    load_threads :: MonadIO m => CoreM m CoreResult
    load_threads = modify (\st'->st'{_l_threads = Loading}) *> next

    cantLoad_threads :: MonadIO m => CoreM m CoreResult
    cantLoad_threads = modify (\st'->st'{_l_threads = CantLoad}) *> done

    fetch_threads :: MonadIO m => CoreM m CoreResult
    fetch_threads = do
      Store{..} <- get
      case _l_m_board of
        Loaded (Just board@BoardPackResponse{..}) -> do
          lr <- api $ getThreadPacks_ByBoardId' boardPackResponseBoardId
          rehtie lr (const cantLoad_threads) $ \ThreadPackResponses{..} -> do
            modify (\st'->st'{
              _l_threads = Loaded $ idmapFrom threadPackResponseThreadId threadPackResponses
            })
            done
        _ -> cantLoad_threads



    load_thread :: MonadIO m => CoreM m CoreResult
    load_thread = modify (\st'->st'{_l_m_thread = Loading}) *> next

    cantLoad_thread :: MonadIO m => CoreM m CoreResult
    cantLoad_thread = modify (\st'->st'{_l_m_thread = CantLoad}) *> done

    fetch_thread :: MonadIO m => ThreadName -> CoreM m CoreResult
    fetch_thread thread_sid = do
      Store{..} <- get
      case _l_m_board of
        Loaded (Just board@BoardPackResponse{..}) -> do
          lr <- api $ ApiS.getThreadPack_ByBoardId' thread_sid boardPackResponseBoardId
          rehtie lr (const cantLoad_thread) $ \thread_pack -> do
            modify (\st'->st'{
              _l_m_thread = Loaded $ Just thread_pack
            })
            done
        _ -> cantLoad_thread






    load_threadPosts_new :: MonadIO m => CoreM m CoreResult
    load_threadPosts_new = modify (\st'->st'{_l_m_threadPost = Loaded Nothing, _m_threadRequest = Just defaultThreadRequest}) *> next

    cantLoad_threadPosts_new :: MonadIO m => CoreM m CoreResult
    cantLoad_threadPosts_new = modify (\st'->st'{_m_threadRequest = Nothing}) *> done



    load_threadPosts :: MonadIO m => CoreM m CoreResult
    load_threadPosts = modify (\st'->st'{_l_threadPosts = Loading}) *> next

    cantLoad_threadPosts :: MonadIO m => CoreM m CoreResult
    cantLoad_threadPosts = modify (\st'->st'{_l_threadPosts = CantLoad}) *> done

    fetch_threadPosts :: MonadIO m => CoreM m CoreResult
    fetch_threadPosts = do
      Store{..} <- get
      case _l_m_thread of
        Loaded (Just thread@ThreadPackResponse{..}) -> do
          lr <- api $ getThreadPostPacks_ByThreadId' threadPackResponseThreadId
          rehtie lr (const cantLoad_threadPosts) $ \ThreadPostPackResponses{..} -> do
            modify (\st'->st'{
              _l_threadPosts = Loaded $ idmapFrom threadPostPackResponseThreadPostId threadPostPackResponses
            })
            done
        _ -> cantLoad_threadPosts



    load_threadPost :: MonadIO m => CoreM m CoreResult
    load_threadPost = modify (\st'->st'{_l_m_threadPost = Loading}) *> next

    cantLoad_threadPost :: MonadIO m => CoreM m CoreResult
    cantLoad_threadPost = modify (\st'->st'{_l_m_threadPost = CantLoad}) *> done

    fetch_threadPost :: MonadIO m => ThreadPostId -> CoreM m CoreResult
    fetch_threadPost post_id = do
      Store{..} <- get
      lr <- api $ getThreadPostPack' post_id
      rehtie lr (const cantLoad_threadPost) $ \post_pack -> do
        modify (\st'->st'{
          _l_m_threadPost = Loaded $ Just post_pack
        })
        done




    load_organizations_new :: MonadIO m => CoreM m CoreResult
    load_organizations_new = modify (\st'->st'{_l_m_organization = Loaded Nothing, _m_organizationRequest = Just defaultOrganizationRequest}) *> next



    load_organizations_index :: MonadIO m => CoreM m CoreResult
    load_organizations_index = modify (\st'->st'{_l_organizations = Loading}) *> next

    cantLoad_organizations_index :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_index = modify (\st'->st'{_l_organizations = CantLoad}) *> done

    fetch_organizations_index :: MonadIO m => CoreM m CoreResult
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





    load_organization_show :: MonadIO m => CoreM m CoreResult
    load_organization_show = do
      load_organization
      load_forums
      next

    fetch_organization_show :: MonadIO m => OrganizationName -> CoreM m CoreResult
    fetch_organization_show org_sid = do
      fetch_organizations_forums_index org_sid





    load_organizations_forums_new :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_new = do
      load_organization
      load_forums_new
      next

    cantLoad_organizations_forums_new :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_new = do
      cantLoad_organization
      cantLoad_forums_new
      done

    fetch_organizations_forums_new :: MonadIO m => OrganizationName -> CoreM m CoreResult
    fetch_organizations_forums_new org_sid = do
      result <- fetch_organization org_sid
      doneDo result $ load_forums_new *> done



    load_organizations_forums_index :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_index = do
      load_organization
      load_forums
      next

    cantLoad_organizations_forums_index :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_index = do
      cantLoad_organization
      cantLoad_forums
      done

    fetch_organizations_forums_index :: MonadIO m => OrganizationName -> CoreM m CoreResult
    fetch_organizations_forums_index org_sid = do
      result <- fetch_organization org_sid
      doneDo result $ do
        Store{..} <- get
        case _l_forums of
          Loading  -> fetch_forums >>= \core_result_ -> basedOn_ core_result_ start next next
          Loaded _ -> done
          _        -> cantLoad_organizations_forums_index



    load_organizations_forums :: MonadIO m => CoreM m CoreResult
    load_organizations_forums = do
      load_organization
      load_forum
      next

    cantLoad_organizations_forums :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums = do
      cantLoad_organization
      cantLoad_forum
      done

    fetch_organizations_forums :: MonadIO m => OrganizationName -> ForumName -> CoreM m CoreResult
    fetch_organizations_forums org_sid forum_sid = do
      result <- fetch_organization org_sid
      doneDo result $ do
        Store{..} <- get
        case _l_m_forum of
          Loading   -> fetch_forum forum_sid >>= \core_result_ -> basedOn_ core_result_ start next next
          Loaded _  -> done
          _         -> cantLoad_organizations_forums



    load_organizations_forums_show :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_show = do
      load_organization
      load_forum
      next

    cantLoad_organizations_forums_show :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_show = do
      cantLoad_organization
      cantLoad_forum
      done

    fetch_organizations_forums_show :: MonadIO m => OrganizationName -> ForumName -> CoreM m CoreResult
    fetch_organizations_forums_show org_sid forum_sid = do
      result <- fetch_organization org_sid
      doneDo result $ do
        Store{..} <- get
        case _l_m_forum of
          Loading   -> fetch_forum forum_sid >>= \core_result_ -> basedOn_ core_result_ start next next
          Loaded _  -> done
          _         -> cantLoad_organizations_forums






    load_organizations_forums_boards_new :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_boards_new = do
      load_organization
      load_forum
      load_boards_new
      next

    cantLoad_organizations_forums_boards_new :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_boards_new = do
      cantLoad_organization
      cantLoad_forum
      cantLoad_boards_new
      done

    fetch_organizations_forums_boards_new :: MonadIO m => OrganizationName -> ForumName -> CoreM m CoreResult
    fetch_organizations_forums_boards_new org_sid forum_sid = do
      Store{..} <- get
      case (_l_m_organization, _l_m_forum) of
        (Loading, Loading)                 -> fetch_organization org_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loading)         -> fetch_forum forum_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _)) -> done
        _                                  -> cantLoad_organizations_forums_boards_new



    load_organizations_forums_boards_index :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_boards_index = do
      load_organization
      load_forum
      load_boards
      next

    cantLoad_organizations_forums_boards_index :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_boards_index = do
      cantLoad_organization
      cantLoad_forum
      cantLoad_boards
      done

    fetch_organizations_forums_boards_index :: MonadIO m => OrganizationName -> ForumName -> CoreM m CoreResult
    fetch_organizations_forums_boards_index org_sid forum_sid = do
      Store{..} <- get
      case (_l_m_organization, _l_m_forum, _l_boards) of
        (Loading, Loading, Loading)                  -> fetch_organization org_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loading, Loading)          -> fetch_forum forum_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loading)  -> fetch_boards >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loaded _) -> done
        _                                            -> cantLoad_organizations_forums_boards_index



    load_organizations_forums_boards :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_boards = do
      load_organization
      load_forum
      load_board
      next

    cantLoad_organizations_forums_boards :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_boards = do
      cantLoad_organization
      cantLoad_forum
      cantLoad_board
      done

    fetch_organizations_forums_boards :: MonadIO m => OrganizationName -> ForumName -> BoardName -> CoreM m CoreResult
    fetch_organizations_forums_boards org_sid forum_sid board_sid = do
      Store{..} <- get
      case (_l_m_organization, _l_m_forum, _l_m_board) of
        (Loading, Loading, Loading)                         -> fetch_organization org_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loading, Loading)                 -> fetch_forum forum_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loading)         -> fetch_board board_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loaded (Just _)) -> done
        _                                                   -> cantLoad_organizations_forums_boards_index



    load_organizations_forums_boards_show :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_boards_show = do
      load_organizations_forums_boards
      load_threads
      next

    cantLoad_organizations_forums_boards_show :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_boards_show = do
      cantLoad_organizations_forums_boards
      cantLoad_threads
      done

    fetch_organizations_forums_boards_show :: MonadIO m => OrganizationName -> ForumName -> BoardName -> CoreM m CoreResult
    fetch_organizations_forums_boards_show org_sid forum_sid board_sid = do
      result <- fetch_organizations_forums_boards org_sid forum_sid board_sid
      doneDo result $ do
        Store{..} <- get
        case _l_m_board of
          Loading   -> fetch_threads >>= \core_result_ -> basedOn_ core_result_ start next next
          Loaded _  -> done
          _         -> cantLoad_organizations_forums_boards_show







    load_organizations_forums_boards_threads_new :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_boards_threads_new = do
      load_organization
      load_forum
      load_board
      load_threads_new
      next

    cantLoad_organizations_forums_boards_threads_new :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_boards_threads_new = do
      cantLoad_organization
      cantLoad_forum
      cantLoad_board
      cantLoad_threads_new
      done

    fetch_organizations_forums_boards_threads_new :: MonadIO m => OrganizationName -> ForumName -> BoardName -> CoreM m CoreResult
    fetch_organizations_forums_boards_threads_new org_sid forum_sid board_sid = do
      Store{..} <- get
      case (_l_m_organization, _l_m_forum, _l_m_board) of
        (Loading, Loading, Loading)                         -> fetch_organization org_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loading, Loading)                 -> fetch_forum forum_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loading)         -> fetch_board board_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loaded (Just _)) -> done
        _                                                   -> cantLoad_organizations_forums_boards_threads_new



    load_organizations_forums_boards_threads_index :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_boards_threads_index = do
      load_organization
      load_forum
      load_board
      load_threads
      next

    cantLoad_organizations_forums_boards_threads_index :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_boards_threads_index = do
      cantLoad_organization
      cantLoad_forum
      cantLoad_board
      cantLoad_threads
      done

    fetch_organizations_forums_boards_threads_index :: MonadIO m => OrganizationName -> ForumName -> BoardName -> CoreM m CoreResult
    fetch_organizations_forums_boards_threads_index org_sid forum_sid board_sid = do
      Store{..} <- get
      case (_l_m_organization, _l_m_forum, _l_m_board, _l_threads) of
        (Loading, Loading, Loading, Loading)                          -> fetch_organization org_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loading, Loading, Loading)                  -> fetch_forum forum_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loading, Loading)          -> fetch_board board_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loaded (Just _), Loading)  -> fetch_threads >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loaded (Just _), Loaded _) -> done
        _                                                             -> cantLoad_organizations_forums_boards_threads_index



    load_organizations_forums_boards_threads :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_boards_threads = do
      load_organization
      load_forum
      load_board
      load_threads
      next

    cantLoad_organizations_forums_boards_threads :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_boards_threads = do
      cantLoad_organization
      cantLoad_forum
      cantLoad_board
      cantLoad_threads
      done

    fetch_organizations_forums_boards_threads :: MonadIO m => OrganizationName -> ForumName -> BoardName -> ThreadName -> CoreM m CoreResult
    fetch_organizations_forums_boards_threads org_sid forum_sid board_sid thread_sid = do
      Store{..} <- get
      case (_l_m_organization, _l_m_forum, _l_m_board, _l_m_thread) of
        (Loading, Loading, Loading, Loading)                                 -> fetch_organization org_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loading, Loading, Loading)                         -> fetch_forum forum_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loading, Loading)                 -> fetch_board board_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loaded (Just _), Loading)         -> fetch_thread thread_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loaded (Just _), Loaded (Just _)) -> done
        _                                                                    -> cantLoad_organizations_forums_boards_threads_index



    load_organizations_forums_boards_threads_show :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_boards_threads_show = do
      load_organization
      load_forum
      next

    cantLoad_organizations_forums_boards_threads_show :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_boards_threads_show = do
      cantLoad_organization
      cantLoad_forum
      done

    fetch_organizations_forums_boards_threads_show :: MonadIO m => OrganizationName -> ForumName -> BoardName -> ThreadName -> CoreM m CoreResult
    fetch_organizations_forums_boards_threads_show org_sid forum_sid board_sid thread_sid = do
      result <- fetch_organizations_forums_boards_threads org_sid forum_sid board_sid thread_sid
      doneDo result $ do
        Store{..} <- get
        case _l_m_threadPost of
          Loading   -> fetch_threadPosts >>= \core_result_ -> basedOn_ core_result_ start next next
          Loaded _  -> done
          _         -> cantLoad_organizations_forums_boards_threads_show





    load_organizations_forums_boards_threads_posts_index :: MonadIO m => CoreM m CoreResult
    load_organizations_forums_boards_threads_posts_index = do
      load_organizations_forums_boards_threads_new
      load_threadPosts
      next

    cantLoad_organizations_forums_boards_threads_posts_index :: MonadIO m => CoreM m CoreResult
    cantLoad_organizations_forums_boards_threads_posts_index = do
      cantLoad_organizations_forums_boards_threads_new
      cantLoad_threadPosts
      done

    fetch_organizations_forums_boards_threads_posts_index :: MonadIO m => OrganizationName -> ForumName -> BoardName -> ThreadName -> CoreM m CoreResult
    fetch_organizations_forums_boards_threads_posts_index org_sid forum_sid board_sid thread_sid = do
      result <- fetch_organizations_forums_boards_threads_new org_sid forum_sid board_sid
      doneDo result $ do
        Store{..} <- get
        case _l_m_thread of
          Loading  -> fetch_threadPosts >>= \core_result_ -> basedOn_ core_result_ start next next
          Loaded _ -> done
          _        -> cantLoad_organizations_forums_boards_threads_index







  act_save = do
    route_with <- gets _route
    case route_with of
      RouteWith (Organizations _) _                          -> act_save_organization
      RouteWith (Users _) _                                  -> done
      RouteWith (OrganizationsForums _ _) _                  -> act_save_forum
      RouteWith (OrganizationsForumsBoards _ _ _) _          -> act_save_board
      RouteWith (OrganizationsForumsBoardsThreads _ _ _ _) _ -> act_save_thread
      -- RouteWith (OrganizationsForumsBoardsThreadPosts _ _) _ -> act_save_thread_post
      _ -> done

    where
    act_save_organization :: MonadIO m => CoreM m CoreResult
    act_save_organization = do
      Store{..} <- get
      case (_l_m_me, _m_organizationRequest) of
        (Loaded (Just UserResponse{..}), Just request) -> do
          lr <- case _l_m_organization of
                  Loaded (Just OrganizationPackResponse{..}) -> api $ putOrganization' organizationPackResponseOrganizationId request
                  _                                          -> api $ postOrganization' (request { organizationRequestEmail = userResponseEmail })
          rehtie lr (const done) $ \OrganizationResponse{..} -> do
            reroute $ RouteWith (Organizations (ShowS organizationResponseName)) emptyParams
        _ ->  done

    act_save_forum :: MonadIO m => CoreM m CoreResult
    act_save_forum = do
      Store{..} <- get
      case (_l_m_organization, _m_forumRequest) of
        (Loaded (Just OrganizationPackResponse{..}), Just request) -> do
          lr <- case _l_m_forum of
                  Loaded (Just ForumPackResponse{..}) -> api $ putForum' forumPackResponseForumId request
                  _                                   -> api $ postForum_ByOrganizationId' organizationPackResponseOrganizationId request
          rehtie lr (const done) $ \ForumResponse{..} -> do
            reroute $ RouteWith (OrganizationsForums (organizationResponseName organizationPackResponseOrganization) (ShowS forumResponseName)) emptyParams
        _ -> done

    act_save_board :: MonadIO m => CoreM m CoreResult
    act_save_board = do
      Store{..} <- get
      case (_l_m_organization, _l_m_forum, _m_boardRequest) of
        (Loaded (Just OrganizationPackResponse{..}), Loaded (Just ForumPackResponse{..}), Just request) -> do
          lr <- case _l_m_board of
                  Loaded (Just BoardPackResponse{..}) -> api $ putBoard' boardPackResponseBoardId request
                  _                                   -> api $ postBoard_ByForumId' forumPackResponseForumId request
          rehtie lr (const done) $ \BoardResponse{..} -> do
            reroute $ RouteWith (OrganizationsForumsBoards (organizationResponseName organizationPackResponseOrganization) (forumResponseName forumPackResponseForum) (ShowS boardResponseName)) emptyParams
        _ -> done

    act_save_thread :: MonadIO m => CoreM m CoreResult
    act_save_thread = do
      Store{..} <- get
      case (_l_m_organization, _l_m_forum, _l_m_board, _m_threadRequest) of
        (Loaded (Just OrganizationPackResponse{..}), Loaded (Just ForumPackResponse{..}), Loaded (Just BoardPackResponse{..}), Just request) -> do
          lr <- case _l_m_thread of
                  Loaded (Just ThreadPackResponse{..}) -> api $ putThread' threadPackResponseThreadId request
                  _                                    -> api $ postThread_ByBoardId' boardPackResponseBoardId request
          rehtie lr (const done) $ \ThreadResponse{..} -> do
            reroute $ RouteWith (OrganizationsForumsBoardsThreads (organizationResponseName organizationPackResponseOrganization) (forumResponseName forumPackResponseForum) (boardResponseName boardPackResponseBoard) (ShowS threadResponseName)) emptyParams
        _ -> done
