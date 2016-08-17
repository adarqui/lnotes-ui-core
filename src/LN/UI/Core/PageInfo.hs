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
  currentOffset  :: !Int64,
  resultsPerPage :: !Int64,
  totalResults   :: !Int64,
  totalPages     :: !Int64,
  sortOrder      :: !SortOrderBy,
  order          :: !OrderBy
} deriving (Show, Generic, Typeable, NFData)



defaultPageInfo :: PageInfo
defaultPageInfo = PageInfo {
  currentPage    = defaultCurrentPage,
  currentOffset  = defaultCurrentOffset,
  resultsPerPage = defaultResultsPerPage,
  totalResults   = defaultTotalResults,
  totalPages     = defaultTotalPages,
  sortOrder      = SortOrderBy_Asc,
  order          = OrderBy_Id
}



defaultCurrentPage :: Int64
defaultCurrentPage = 1

defaultCurrentOffset :: Int64
defaultCurrentOffset = 0

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
    currentPage    = current_page,
    currentOffset  = current_offset,
    resultsPerPage = results_per_page,
    totalResults   = 0,
    totalPages     = 1,
    sortOrder      = sort_order,
    order          = order
  }
  where

  m_offset         = Map.lookup ParamTag_Offset params
  m_limit          = Map.lookup ParamTag_Limit params
  m_sort_order     = Map.lookup ParamTag_SortOrder params
  m_order          = Map.lookup ParamTag_Order params

  current_page     = 0
  current_offset   = maybe defaultCurrentPage (\(Offset offset) -> offset) m_offset
  results_per_page = maybe defaultResultsPerPage (\(Limit limit) -> limit) m_limit
  total_results    = 0
  total_pages      = 1
  order            = maybe defaultOrder (\(Order order) -> order) m_order
  sort_order       = maybe defaultSortOrder (\(SortOrder sort_order) -> sort_order) m_sort_order



paramsFromPageInfo :: PageInfo -> [Param]
paramsFromPageInfo PageInfo{..} =
  [ Offset currentOffset             -- ^ we're either page 1 or 1+n .. offsets start at 0
  , Limit resultsPerPage
  , SortOrder sortOrder, Order order
  ]



runPageInfo :: CountResponses -> PageInfo -> PageInfo
runPageInfo CountResponses{..} page_info@PageInfo{..} =
  case countResponses of
    (CountResponse{..}:[]) ->
      page_info {
        currentPage  =
          (let
            div_pages = currentOffset `div` resultsPerPage
            rem_pages = currentOffset `rem` resultsPerPage
          in div_pages + 1),
        totalResults = countResponseN,
        totalPages   =
          let
            div_pages = countResponseN `div` resultsPerPage
            rem_pages = countResponseN `rem` resultsPerPage
          in div_pages + (if rem_pages > 0 then 1 else 0)
      }
    _      -> page_info
