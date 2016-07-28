module LN.UI.Core.PageNumbersSpec (
  main,
  spec
) where



import           LN.UI.Core.PageInfo
import           LN.UI.Core.PageNumbers
import           LN.UI.Core.Router.Route
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "pageRange tests" $ do
    it "should provide a proper range" $ do
      pageRange page_info  `shouldBe` []
      pageRange page_info2 `shouldBe` [1..totalPages page_info2]

  describe "buildPages tests" $ do
    it "should provide a proper Pages result" $ do
      buildPages page_info (routeWith' Home) `shouldBe` (1, [], 1, 20)

  where
  page_info  = defaultPageInfo
  page_info2 = page_info{ totalPages = 20 }
