{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Core.Helpers.WebRoutes (
    str1
  , nostr
  , notCRUD
) where



import           Data.Text        (Text)
import qualified Data.Text        as Text (null)
import           Text.Parsec.Prim ((<?>))
import           Web.Routes



str1 :: URLParser Text
str1 = (pToken (const ()) (\y -> if (not $ Text.null y) then Just y else Nothing)) <?> "String is empty"



nostr :: URLParser Text
nostr = (pToken (const ()) (\y -> if (Text.null y) then Just y else Nothing)) <?> "String is not empty"



notCRUD :: URLParser Text
notCRUD = (pToken (const ()) (\y -> if (not $ any (==y) ["_new","_edit","_delete"]) then Just y else Nothing)) <?> "String contains CRUD"
