module LN.UI.Core.PageInfoSpec (
  main,
  spec
) where



import           Test.Hspec

import           LN.UI.Core.PageInfo
import           LN.UI.Core.PageInfo
import           LN.UI.Core.Router.Route
import           LN.T



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "runPageInfo tests" $ do
    it "should provide proper PageInfo results" $ do
      currentPage rpi1   `shouldBe` 1
      currentOffset rpi1 `shouldBe` 0
      totalResults rpi1  `shouldBe` 100
      totalPages rpi1    `shouldBe` 5

      currentPage rpi2   `shouldBe` 16
      currentOffset rpi2 `shouldBe` 300
      totalResults rpi2  `shouldBe` 539
      totalPages rpi2    `shouldBe` 27

      currentPage rpi3   `shouldBe` 42
      currentOffset rpi3 `shouldBe` 820
      totalResults rpi3  `shouldBe` 1576
      totalPages rpi3    `shouldBe` 79

  where
  page_info1  = PageInfo {
                 currentPage    = 1
               , currentOffset  = 0
               , resultsPerPage = 20
               , totalResults   = 0
               , totalPages     = 1
               , sortOrder      = SortOrderBy_Asc
               , order          = OrderBy_Id
               }
  counts1 = CountResponses [CountResponse 0 100]
  rpi1 = runPageInfo counts1 page_info1

  page_info2  = PageInfo {
                 currentPage    = 1
               , currentOffset  = 300
               , resultsPerPage = 20
               , totalResults   = 0
               , totalPages     = 1
               , sortOrder      = SortOrderBy_Asc
               , order          = OrderBy_Id
               }
  counts2 = CountResponses [CountResponse 0 539]
  rpi2 = runPageInfo counts2 page_info2

  page_info3  = PageInfo {
                 currentPage    = 1
               , currentOffset  = 820
               , resultsPerPage = 20
               , totalResults   = 0
               , totalPages     = 1
               , sortOrder      = SortOrderBy_Asc
               , order          = OrderBy_Id
               }
  counts3 = CountResponses [CountResponse 0 1576]
  rpi3 = runPageInfo counts3 page_info3
