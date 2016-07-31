{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module LN.UI.Core.App.Tag (
    addTag
  , deleteTag
) where



import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)

import           LN.UI.Core.Helpers.DataList (deleteNth)



addTag :: [Text]  -> Maybe Text -> ([Text], Maybe Text)
addTag tags m_tag =
  (maybe tags (\tag -> tags <> [tag]) m_tag, Nothing)



deleteTag :: [Text] -> Int -> [Text]
deleteTag tags idx =
  deleteNth idx tags
