{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module LN.UI.Core.Router.CRUD (
  CRUD (..),
  TyCRUD (..)
) where



import           Control.Applicative
import           Control.DeepSeq                          (NFData)
import           Data.Int                                 (Int64)
import           Data.Text                                (Text)
import qualified Data.Text                                as Text
import           Prelude                                  (Bool (..), Eq, Show,
                                                           show, ($))
import           Text.ParserCombinators.Parsec.Prim       (try, (<?>))
import           Web.Routes

import           LN.UI.Core.Helpers.WebRoutes             (nostr, str1)
import           LN.UI.Core.Router.Link                   ()
import           LN.UI.Core.Router.LinkName               (HasLinkName,
                                                           linkName)



data CRUD
  = Index
  | ShowS Text
  | ShowI Int64
  | ShowB Bool
  | New
  | EditS Text
  | EditI Int64
  | DeleteS Text
  | DeleteI Int64
  | DeleteZ
  deriving (Eq, Show, Generic, NFData)



instance PathInfo CRUD where
  toPathSegments crud =
    case crud of
      Index     -> [""]
      New       -> ["_new"]
      ShowS s   -> [s]
      ShowI i   -> [Text.pack $ show i]
      ShowB b   -> [bool2Text b]
      EditS s   -> ["_edit", s]
      EditI i   -> ["_edit", Text.pack $ show i]
      DeleteS s -> ["_delete", s]
      DeleteI i -> ["_delete", Text.pack $ show i]
      DeleteZ   -> ["_delete"]
  fromPathSegments =
        (New     <$  segment "_new"

    -- TODO FIXME: This is hideous.
    <|> (do
            segment "_edit"
            (EditI <$> fromPathSegments) <|> (EditS <$> str1))
    -- TODO FIXME: This is hideous.
    <|> (do
            segment "_delete"
            (DeleteI <$> fromPathSegments) <|> (DeleteS <$> str1) <|> (pure DeleteZ))

    <|> ShowI <$> fromPathSegments
    <|> ShowB <$> fromPathSegments
    <|> ShowS <$> str1
    <|> pure Index)
    <?> "CRUD: fromPathSegments failed"
    -- TODO FIXME: Can't do Index <$ segment "" because it fails to pattern match. Though, pure Index works because it's terminal.



bool2Text :: Bool -> Text
bool2Text True  = "true"
bool2Text False = "false"



-- text2Bool :: Text -> Bool
-- text2Bool "true" = True
-- text2Bool _      = False



instance PathInfo Bool where
  toPathSegments bool =
    case bool of
      True  -> ["true"]
      False -> ["false"]
  fromPathSegments =
        (True <$ segment "true"
    <|> False <$ segment "false")
    <?> "Boo: fromPathSegments failed"



data TyCRUD
  = TyCreate
  | TyRead
  | TyUpdate
  | TyDelete
  deriving (Eq, Generic)



instance HasLinkName TyCRUD where
  linkName TyCreate = "Create"
  linkName TyRead   = "View"
  linkName TyUpdate = "Edit"
  linkName TyDelete = "Delete"
