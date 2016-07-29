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
    , _l_m_organizationRequest :: Loader (Maybe OrganizationRequest)
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
      , _l_m_organizationRequest = Loaded Nothing
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
  | SetState Store
  | Nop
  deriving (Typeable, Generic, NFData)
