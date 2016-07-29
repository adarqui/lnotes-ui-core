{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.Core.State.Internal (
  Store (..),
  defaultStore,
  ImmutableStore (..),
  defaultImmutableStore,
  Action (..)
) where



import           Control.DeepSeq            (NFData)
import           Data.Int                   (Int64)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Haskell.Api.Helpers
import           Haskell.Api.Helpers.Shared

import           LN.T
import           LN.UI.Core.Loader          (Loader (..))
import           LN.UI.Core.PageInfo        (PageInfo, defaultPageInfo)
import           LN.UI.Core.Router          (Route (..), RouteWith, routeWith')
import           LN.UI.Core.Types



data Store = Store {
      _route                   :: RouteWith
    , _pageInfo                :: PageInfo
    , _l_m_me                  :: Loader (Maybe UserResponse)
    , _meId                    :: UserId
    , _usersCache              :: Map UserId UserSanitizedPackResponse
    , _l_organizations         :: Loader (Map OrganizationId OrganizationPackResponse)
    , _l_users                 :: Loader (Map UserId UserSanitizedPackResponse)
    , _l_forums                :: Loader (Map ForumId ForumPackResponse)
    , _l_boards                :: Loader (Map BoardId BoardPackResponse)
    , _l_threads               :: Loader (Map ThreadId ThreadPackResponse)
    , _l_posts                 :: Loader (Map ThreadId ThreadPostPackResponse)
    , _l_m_organization        :: Loader (Maybe OrganizationPackResponse)
    , _l_m_organizationRequest :: Loader (Maybe OrganizationRequest)
    , _l_m_user                :: Loader (Maybe UserPackResponse)
    , _l_m_forum               :: Loader (Maybe ForumPackResponse)
    , _l_m_forumRequest        :: Loader (Maybe ForumRequest)
    , _l_m_board               :: Loader (Maybe BoardPackResponse)
    , _l_m_boardRequest        :: Loader (Maybe BoardRequest)
    , _l_m_thread              :: Loader (Maybe ThreadPackResponse)
    , _l_m_threadRequest       :: Loader (Maybe ThreadRequest)
    , _l_m_threadPost          :: Loader (Maybe ThreadPostPackResponse)
    , _l_m_threadPostRequest   :: Loader (Maybe ThreadPostRequest)
    } deriving (Typeable, Generic, NFData)



defaultStore :: Store
defaultStore = Store {
        _route                   = routeWith' Home
      , _pageInfo                = defaultPageInfo
      , _l_m_me                  = Loaded Nothing
      , _meId                    = 0
      , _usersCache              = Map.empty
      , _l_organizations         = Loaded Map.empty
      , _l_users                 = Loaded Map.empty
      , _l_forums                = Loaded Map.empty
      , _l_boards                = Loaded Map.empty
      , _l_threads               = Loaded Map.empty
      , _l_posts                 = Loaded Map.empty
      , _l_m_organization        = Loaded Nothing
      , _l_m_organizationRequest = Loaded Nothing
      , _l_m_user                = Loaded Nothing
      , _l_m_forum               = Loaded Nothing
      , _l_m_forumRequest        = Loaded Nothing
      , _l_m_board               = Loaded Nothing
      , _l_m_boardRequest        = Loaded Nothing
      , _l_m_thread              = Loaded Nothing
      , _l_m_threadRequest       = Loaded Nothing
      , _l_m_threadPost          = Loaded Nothing
      , _l_m_threadPostRequest   = Loaded Nothing
    }



data ImmutableStore = ImmutableStore {
    _apiOptions :: ApiOptions SpecificApiOptions
  } deriving (Typeable, Generic)



defaultImmutableStore :: ImmutableStore
defaultImmutableStore = ImmutableStore {
  _apiOptions = (defaultApiOptions :: ApiOptions SpecificApiOptions)
}



data Action
  = Init
  | Route RouteWith
  | SyncUsers [Int64]
  | ApplyState (Store -> Store)
  | Nop
  deriving (Typeable, Generic, NFData)
