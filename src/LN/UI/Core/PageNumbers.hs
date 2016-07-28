{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.Core.PageNumbers (
  pageRange,
  buildPages,
  runPageInfo
) where



import           Data.Int            (Int64)

import           LN.T.Count          (CountResponse (..), CountResponses (..))
import           LN.UI.Core.PageInfo (PageInfo (..))
import           LN.UI.Core.Router   (RouteWith (..))



type Pages =
  (Int64   -- prev
  ,[Int64] -- pages
  ,Int64   -- next
  ,Int64   -- limit
  )



-- | If only one page exists, we consider it empty.
--
pageRange :: PageInfo -> [Int64]
pageRange PageInfo{..} =
  case [1..totalPages] of
    [_] -> []
    xs  -> xs



buildPages :: PageInfo -> RouteWith -> Pages
buildPages page_info@PageInfo{..} _ =
  ( prev
  , pageRange page_info
  , next
  , resultsPerPage
  )
  where
  prev = let p = (currentPage - 1) in if p < 1 then 1 else p
  next = let p = (currentPage + 1) in if (p > totalPages) then totalPages else p



runPageInfo :: CountResponses -> PageInfo -> PageInfo
runPageInfo CountResponses{..} page_info =
  case countResponses of
    (CountResponse{..}:[]) ->
      page_info {
        totalResults = countResponseN,
        totalPages   = (countResponseN `div` resultsPerPage page_info)
      }
    _      -> page_info
