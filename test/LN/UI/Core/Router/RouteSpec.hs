{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Core.Router.RouteSpec (
  main,
  spec
) where



import           LN.T.Param
import           LN.UI.Core.Router.CRUD
import           LN.UI.Core.Router.Param
import           LN.UI.Core.Router.Route
import           Test.Hspec
import           Web.Routes



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "routes tests" $ do
    it "toPathInfo should work" $ do
      toPathInfo Home                           `shouldBe` "/"
      toPathInfo About                          `shouldBe` "/about"
      toPathInfo Me                             `shouldBe` "/me"
      toPathInfo Errors                         `shouldBe` "/errors"
      toPathInfo Portal                         `shouldBe` "/portal"
      toPathInfo (Organizations Index)          `shouldBe` "/organizations"
      toPathInfo (Organizations New)            `shouldBe` "/organizations/_new"
      toPathInfo (Organizations (ShowS "ln"))   `shouldBe` "/ln"
      toPathInfo (Organizations (EditS "ln"))   `shouldBe` "/organizations/_edit/ln"
      toPathInfo (Organizations (DeleteS "ln")) `shouldBe` "/organizations/_delete/ln"

      toPathInfo (OrganizationsForums "ln" Index)               `shouldBe` "/ln/f"
      toPathInfo (OrganizationsForums "ln" New)                 `shouldBe` "/ln/f/_new"
      toPathInfo (OrganizationsForums "ln" (ShowS "community")) `shouldBe` "/ln/f/community"

      toPathInfo (OrganizationsForumsBoards "ln" "community" Index)                  `shouldBe` "/ln/f/community"
      toPathInfo (OrganizationsForumsBoards "ln" "community" (ShowS "board_parent")) `shouldBe` "/ln/f/community/board_parent"
      toPathInfo (OrganizationsForumsBoardsThreads "ln" "community" "board_parent" (ShowS "thread")) `shouldBe` "/ln/f/community/board_parent/thread"
      toPathInfo (OrganizationsForumsBoardsThreadsPosts "ln" "community" "board_parent" "thread" (ShowI 1)) `shouldBe` "/ln/f/community/board_parent/thread/1"

    it "fromPathInfo should work" $ do
      fromPathInfo "/"                         `shouldBe` (Right Home)
      fromPathInfo "/about"                    `shouldBe` (Right About)
      fromPathInfo "/me"                       `shouldBe` (Right Me)
      fromPathInfo "/errors"                   `shouldBe` (Right Errors)
      fromPathInfo "/portal"                   `shouldBe` (Right Portal)
      fromPathInfo "/organizations"            `shouldBe` (Right $ Organizations Index)
      fromPathInfo "/organizations/_new"       `shouldBe` (Right $ Organizations New)
      fromPathInfo "/ln"                       `shouldBe` (Right $ Organizations (ShowS "ln"))
      fromPathInfo "/organizations/_edit/ln"   `shouldBe` (Right $ Organizations (EditS "ln"))
      fromPathInfo "/organizations/_delete/ln" `shouldBe` (Right $ Organizations (DeleteS "ln"))
      fromPathInfo "/ln/f"                     `shouldBe` (Right $ OrganizationsForums "ln" Index)
      fromPathInfo "/ln/f/_new"                `shouldBe` (Right $ OrganizationsForums "ln" New)
      fromPathInfo "/ln/f/_edit/community"     `shouldBe` (Right $ OrganizationsForums "ln" (EditS "community"))
      fromPathInfo "/ln/f/_delete/community"   `shouldBe` (Right $ OrganizationsForums "ln" (DeleteS "community"))
      fromPathInfo "/ln/f/community"           `shouldBe` (Right $ OrganizationsForums "ln" (ShowS "community"))
      fromPathInfo "/ln/f/community/_new"      `shouldBe` (Right $ OrganizationsForumsBoards "ln" "community" New)
      -- fromPathInfo "/ln/f/community/board_parent" `shouldBe` (Right $ OrganizationsForumsBoards "ln" "community" (ShowS "board_parent"))
      -- fromPathInfo "/ln/f/community/board_parent/thread" `shouldBe` (Right $ OrganizationsForumsBoardsThreads "ln" "community" "board_parent" (ShowS "thread"))
      -- fromPathInfo "/ln/f/community/board_parent/thread/1" `shouldBe` (Right $ OrganizationsForumsBoardsThreadsPosts "ln" "community" "board_parent" "threads" (ShowI 1))


  describe "route with tests" $ do
    it "toRouteWith should gives us a proper RouteWith" $ do
      toRouteWith "/about"
        `shouldBe` (RouteWith About emptyParams)

      toRouteWith "/about?limit=1&offset=2"
        `shouldBe` (RouteWith About $ buildParams [(ParamTag_Limit, Limit 1), (ParamTag_Offset, Offset 2)])

    it "fromRouteWith should give us a proper url string" $ do
      fromRouteWith (routeWith' About)
        `shouldBe` "/about"

      fromRouteWith (routeWith About [(ParamTag_Limit, Limit 1), (ParamTag_Offset, Offset 2)])
        `shouldBe` "/about?limit=1&offset=2"

    --
    -- HASHES
    --

    it "toRouteWithHash should gives us a proper RouteWith" $ do
      toRouteWithHash "#/about"
        `shouldBe` (RouteWith About emptyParams)

      toRouteWithHash "#/about?limit=1&offset=2"
        `shouldBe` (RouteWith About $ buildParams [(ParamTag_Limit, Limit 1), (ParamTag_Offset, Offset 2)])

    it "fromRouteWithHash should give us a proper url string" $ do
      fromRouteWithHash (routeWith' About)
        `shouldBe` "#/about"

      fromRouteWithHash (routeWith About [(ParamTag_Limit, Limit 1), (ParamTag_Offset, Offset 2)])
        `shouldBe` "#/about?limit=1&offset=2"
