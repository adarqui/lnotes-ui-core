{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module LN.UI.Core.Router.Route (
  RouteWith (..),
  routeWith,
  routeWith',
  fromRouteWith,
  fromRouteWithHash,
  toRouteWith,
  toRouteWithHash,
  Route (..),
  HasLinkName,
  linkName,
  HasCrumb,
  crumb
) where



import           Control.Applicative          ((*>), (<$), (<$>), (<*>), (<|>))
import           Control.DeepSeq              (NFData)
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as BSC
import           Data.Either                  (rights)
import           Data.Either                  (Either (..))
import           Data.List                    (scanl)
import qualified Data.Map                     as Map
import           Data.Maybe                   (Maybe (Just))
import           Data.Monoid                  (mempty, (<>))
import           Data.Text                    (Text)
import           Haskell.Api.Helpers.Shared   (qp)
import           Prelude                      (Eq, Int, Show, fmap, map, pure,
                                               ($), (.), (==), (>>=))
import           Text.Parsec.Prim             (try, (<?>))
import           Web.Routes

import           LN.T
import           LN.UI.Core.Helpers.DataList  (tailFriendly)
import           LN.UI.Core.Helpers.DataText  (tshow)
import           LN.UI.Core.Helpers.GHCJS     (JSString, textToJSString')
import           LN.UI.Core.Helpers.WebRoutes (notCRUD, notCRUDstr1, str1)
import           LN.UI.Core.Router.CRUD       (CRUD (..))
import           LN.UI.Core.Router.Crumb      (HasCrumb, crumb)
import           LN.UI.Core.Router.LinkName   (HasLinkName, linkName)
import           LN.UI.Core.Router.Param      (Params, buildParams,
                                               fromWebRoutesParams)



data RouteWith
  = RouteWith Route Params
  deriving (Eq, Show, Generic, NFData)



routeWith :: Route -> [(ParamTag, Param)] -> RouteWith
routeWith route params = RouteWith route (buildParams params)



routeWith' :: Route -> RouteWith
routeWith' route = routeWith route mempty



fromRouteWith :: RouteWith -> Text
fromRouteWith (RouteWith route params) =
  toPathInfoParams route params'
  where
  params' = map (fmap Just . qp) $ Map.elems params



fromRouteWithHash :: RouteWith -> JSString
fromRouteWithHash = textToJSString' . ("#" <>) <$> fromRouteWith



toRouteWith :: ByteString -> RouteWith
toRouteWith url =
  case (fromPathInfoParams url) of
    Left _               -> routeWith' NotFound
    Right (url_, params) -> routeWith url_ $ fromWebRoutesParams params



toRouteWithHash :: ByteString -> RouteWith
toRouteWithHash = toRouteWith . BSC.drop 1



data Route
  = Home
  | About
  | Me
  | Errors
  | Portal
  | Organizations CRUD
  | OrganizationsForums Text CRUD
  | OrganizationsForumsBoards Text Text CRUD
  | OrganizationsForumsBoardsThreads Text Text Text CRUD
  | OrganizationsForumsBoardsThreadsPosts Text Text Text Text CRUD
  | OrganizationsTeams Text CRUD
  | OrganizationsTeamsMembers Text Text CRUD
  | OrganizationsMembersOnly Text
  | OrganizationsMembership Text CRUD
  | Users CRUD
  | UsersProfile Text
  | UsersSettings Text
  | UsersPMs Text
  | UsersThreads Text
  | UsersThreadPosts Text
  | UsersWorkouts Text
  | UsersResources Text
  | UsersLeurons Text
  | UsersLikes Text
  | Resources CRUD
  | ResourcesLeurons Int CRUD
  | ResourcesSiftLeurons Int
  | ResourcesSiftLeuronsLinear Int CRUD
  | ResourcesSiftLeuronsRandom Int
  | Login
  | Logout
  | NotFound
  deriving (Eq, Show, Generic, NFData)



instance HasLinkName Route where
  linkName route = case route of
    Home                            -> "Home"
    About                           -> "About"
    Portal                          -> "Portal"
    Organizations Index             -> "Organizations"
    Organizations New               -> "New"
    Organizations (ShowS org_sid)   -> org_sid
    Organizations (EditS org_sid)   -> org_sid
    Organizations (DeleteS org_sid) -> org_sid
    OrganizationsForums _ Index               -> "Forums"
    OrganizationsForums _ New                 -> "New"
    OrganizationsForums _ (ShowS forum_sid)   -> forum_sid
    OrganizationsForums _ (EditS forum_sid)   -> forum_sid
    OrganizationsForums _ (DeleteS forum_sid) -> forum_sid
    OrganizationsForumsBoards _ _ Index               -> "Boards"
    OrganizationsForumsBoards _ _ New                 -> "New"
    OrganizationsForumsBoards _ _ (ShowS board_sid)   -> board_sid
    OrganizationsForumsBoards _ _ (EditS board_sid)   -> board_sid
    OrganizationsForumsBoards _ _ (DeleteS board_sid) -> board_sid
    OrganizationsForumsBoardsThreads _ _ _ Index               -> "Threads"
    OrganizationsForumsBoardsThreads _ _ _ New                 -> "New"
    OrganizationsForumsBoardsThreads _ _ _ (ShowS thread_sid)   -> thread_sid
    OrganizationsForumsBoardsThreads _ _ _ (EditS thread_sid)   -> thread_sid
    OrganizationsForumsBoardsThreads _ _ _ (DeleteS thread_sid) -> thread_sid
    OrganizationsForumsBoardsThreadsPosts _ _ _ _ Index             -> "Posts"
    OrganizationsForumsBoardsThreadsPosts _ _ _ _ New               -> "New"
    OrganizationsForumsBoardsThreadsPosts _ _ _ _ (ShowI post_id)   -> tshow post_id
    OrganizationsForumsBoardsThreadsPosts _ _ _ _ (EditI post_id)   -> tshow post_id
    OrganizationsForumsBoardsThreadsPosts _ _ _ _ (DeleteI post_id) -> tshow post_id
    (Users Index)                   -> "Users"
    Users (ShowS user_sid)          -> user_sid
    Users (EditS user_sid)          -> user_sid
    Users (DeleteS user_sid)        -> user_sid
    Login                           -> "Login"
    Logout                          -> "Logout"
    _                               -> "Unknown"



instance HasLinkName RouteWith where
  linkName (RouteWith route _) = linkName route



instance HasCrumb Route where

  crumb route = maybe_organizations_index <> routes
    where
    segments                   = toPathSegments route
    segment_buckets            = tailFriendly $ scanl (\acc x -> acc <> [x]) [] segments
    routes                     = rights $ map (parseSegments fromPathSegments) segment_buckets
    maybe_organizations_index =
      case routes of
        (r:_) -> case r of
          Organizations Index                             -> []
          Organizations _                                 -> [Organizations Index]
          OrganizationsForums _ _                         -> [Organizations Index]
          OrganizationsForumsBoards _ _ _                 -> [Organizations Index]
          OrganizationsForumsBoardsThreads _ _ _ _        -> [Organizations Index]
          OrganizationsForumsBoardsThreadsPosts _ _ _ _ _ -> [Organizations Index]
          _                                               -> []

        _     -> []



instance PathInfo Route where

  toPathSegments route = case route of
    Home                     -> pure ""
    About                    -> pure "about"
    Me                       -> pure "me"
    Errors                   -> pure "errors"
    Portal                   -> pure "portal"
    Organizations Index      -> pure "organizations"
    Organizations (ShowS s)  -> pure s
    Organizations crud       -> (pure "organizations") <> toPathSegments crud
    OrganizationsForums org_sid Index -> (pure org_sid) <> pure "f"
    OrganizationsForums org_sid crud -> (pure org_sid) <> pure "f" <> toPathSegments crud
    OrganizationsForumsBoards org_sid forum_sid Index -> (pure org_sid) <> pure "f" <> pure forum_sid
    OrganizationsForumsBoards org_sid forum_sid crud -> (pure org_sid) <> pure "f" <> pure forum_sid <> toPathSegments crud
    OrganizationsForumsBoardsThreads org_sid forum_sid board_sid Index -> (pure org_sid) <> pure "f" <> pure forum_sid <> pure board_sid
    OrganizationsForumsBoardsThreads org_sid forum_sid board_sid crud -> (pure org_sid) <> pure "f" <> pure forum_sid <> pure board_sid <> toPathSegments crud
    OrganizationsForumsBoardsThreadsPosts org_sid forum_sid board_sid thread_sid Index -> (pure org_sid) <> pure "f" <> pure forum_sid <> pure board_sid <> pure thread_sid
    OrganizationsForumsBoardsThreadsPosts org_sid forum_sid board_sid thread_sid crud -> (pure org_sid) <> pure "f" <> pure forum_sid <> pure board_sid <> pure thread_sid <> toPathSegments crud
    Users Index              -> pure "users"
    Users crud               -> (pure $ "users") <> toPathSegments crud
    _                        -> pure ""

  fromPathSegments =
        (About         <$ segment "about"
    <|> Me            <$ segment "me"
    <|> Errors        <$ segment "errors"
    <|> Portal        <$ segment "portal"
    <|> Users         <$ segment "users" <*> fromPathSegments
    <|> Organizations <$ segment "organizations" <*> fromPathSegments

    -- welcome to the inferno
    --
    -- This is what you call, the definition of a massive hackjob.
    --
    -- It's nearly 7 AM and I still can't figure this out..
    --
    <|> (do
           org_sid <- notCRUDstr1 -- Organizations
           (do
              segment "f"
              (do
                 forum_sid <- notCRUDstr1 -- OrganizationsForums
                 (do
                    board_sid <- notCRUDstr1 -- OrganizationsForumsBoards
                    (do
                       thread_sid <- notCRUDstr1 -- OrganizationsForumsBoardsThreads
                       fromPathSegments >>= \k -> if k == Index then (OrganizationsForumsBoardsThreads <$> pure org_sid <*> pure forum_sid <*> pure board_sid <*> pure (ShowS thread_sid)) else (OrganizationsForumsBoardsThreadsPosts <$> pure org_sid <*> pure forum_sid <*> pure board_sid <*> pure thread_sid <*> pure k))     <|>     (fromPathSegments >>= \k -> if k == Index then (OrganizationsForumsBoards <$> pure org_sid <*> pure forum_sid <*> pure (ShowS board_sid)) else (OrganizationsForumsBoardsThreads <$> pure org_sid <*> pure forum_sid <*> pure board_sid <*> pure k))) <|> (fromPathSegments >>= \k -> if k == Index then (OrganizationsForums <$> pure org_sid <*> pure (ShowS forum_sid)) else OrganizationsForumsBoards <$> pure org_sid <*> pure forum_sid <*> pure k)) <|> OrganizationsForums <$> pure org_sid <*> fromPathSegments) <|> Organizations <$> (pure (ShowS org_sid)))

    <|> pure Home)
    <?> "Route: fromPathSegments failed"
    -- TODO FIXME: Can't do Home <$ segment "" because it fails to pattern match. Though, pure Index works because it's terminal.
