{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module LN.UI.Core.PageInfo (
  PageInfo (..),
  defaultPageInfo,
  pageInfoFromParams,
  paramsFromPageInfo,
  runPageInfo
) where



import           Control.DeepSeq         (NFData)
import           Data.Int                (Int64)
import qualified Data.Map                as Map (lookup)
import           Data.Maybe              (maybe)
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)
import           Prelude                 hiding (maybe)

import           LN.T                    (OrderBy (..), OrderBy (..),
                                          Param (..), ParamTag (..), CountResponse (..), CountResponses(..),
                                          SortOrderBy (..), SortOrderBy (..))
import           LN.UI.Core.Router.Param (Params)



data PageInfo = PageInfo {
  currentPage    :: !Int64,
  resultsPerPage :: !Int64,
  totalResults   :: !Int64,
  totalPages     :: !Int64,
  sortOrder      :: !SortOrderBy,
  order          :: !OrderBy
} deriving (Show, Generic, Typeable, NFData)



defaultPageInfo :: PageInfo
defaultPageInfo = PageInfo {
  currentPage    = defaultCurrentPage,
  resultsPerPage = defaultResultsPerPage,
  totalResults   = defaultTotalResults,
  totalPages     = defaultTotalPages,
  sortOrder      = SortOrderBy_Asc,
  order          = OrderBy_Id
}



defaultCurrentPage :: Int64
defaultCurrentPage = 1

defaultResultsPerPage :: Int64
defaultResultsPerPage = 20

defaultTotalResults :: Int64
defaultTotalResults = 0

defaultTotalPages :: Int64
defaultTotalPages = 1

defaultSortOrder :: SortOrderBy
defaultSortOrder = SortOrderBy_Asc

defaultOrder :: OrderBy
defaultOrder = OrderBy_Id



pageInfoFromParams :: Params -> PageInfo
pageInfoFromParams params =
  PageInfo {
    currentPage    = maybe defaultCurrentPage (\(Offset offset) -> offset) m_offset,
    resultsPerPage = maybe defaultResultsPerPage (\(Limit limit) -> limit) m_limit,
    totalResults   = 0,
    totalPages     = 1,
    sortOrder      = maybe defaultSortOrder (\(SortOrder sort_order) -> sort_order) m_sort_order,
    order          = maybe defaultOrder (\(Order order) -> order) m_order
  }
  where
  m_offset     = Map.lookup ParamTag_Offset params
  m_limit      = Map.lookup ParamTag_Limit params
  m_sort_order = Map.lookup ParamTag_SortOrder params
  m_order      = Map.lookup ParamTag_Order params



paramsFromPageInfo :: PageInfo -> [Param]
paramsFromPageInfo PageInfo{..} =
  [ Offset (currentPage - 1)         -- ^ we're either page 1 or 1+n .. offsets start at 0
  , Limit resultsPerPage
  , SortOrder sortOrder, Order order
  ]



runPageInfo :: CountResponses -> PageInfo -> PageInfo
runPageInfo CountResponses{..} page_info =
  case countResponses of
    (CountResponse{..}:[]) ->
      page_info {
        totalResults = countResponseN,
        totalPages   =
          let
            div_pages = countResponseN `div` resultsPerPage page_info
            rem_pages = countResponseN `rem` resultsPerPage page_info
          in div_pages + (if rem_pages > 0 then 1 else 0)
      }
    _      -> page_info
