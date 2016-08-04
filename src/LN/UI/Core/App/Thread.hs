{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module LN.UI.Core.App.Thread (
    setDisplayName
  , setDescription
  , clearDescription
  , setSticky
  , setLocked
  , setTag
  , addTag
  , deleteTag
  , clearTags
) where



import           Data.Text          (Text)

import           LN.T
import qualified LN.UI.Core.App.Tag as Tag
import           LN.UI.Core.State



setDisplayName :: ThreadRequest -> Text -> Action
setDisplayName request@ThreadRequest{..} input =
  ApplyState (\st->st{_m_threadRequest = Just $! request{threadRequestDisplayName = input}})



setDescription :: ThreadRequest -> Text -> Action
setDescription request@ThreadRequest{..} input =
  ApplyState (\st->st{_m_threadRequest = Just $! request{threadRequestDescription = Just $! input}})



clearDescription :: ThreadRequest -> Action
clearDescription request@ThreadRequest{..} =
  ApplyState (\st->st{_m_threadRequest = Just $! request{threadRequestDescription = Nothing }})



setSticky :: ThreadRequest -> Bool -> Action
setSticky request@ThreadRequest{..} input =
  ApplyState (\st->st{_m_threadRequest = Just $! request{threadRequestSticky = input }})



setLocked :: ThreadRequest -> Bool -> Action
setLocked request@ThreadRequest{..} input =
  ApplyState (\st->st{_m_threadRequest = Just $! request{threadRequestLocked = input }})



setTag :: ThreadRequest -> Text -> Action
setTag request@ThreadRequest{..} input =
   ApplyState (\st->
     st{
       _m_threadRequest = Just $! request{threadRequestStateTag = Just input}
     })



addTag :: ThreadRequest -> Action
addTag request@ThreadRequest{..} =
  ApplyState (\st->
    st{
      _m_threadRequest = Just $! request{threadRequestTags = tags, threadRequestStateTag = Nothing}
    })
  where
  (tags, _) = Tag.addTag threadRequestTags threadRequestStateTag



deleteTag :: ThreadRequest -> Int -> Action
deleteTag request@ThreadRequest{..} idx =
  ApplyState (\st->
    st{
      _m_threadRequest = Just $! request{threadRequestTags = tags}
    })
  where
  tags = Tag.deleteTag threadRequestTags idx



clearTags :: ThreadRequest -> Action
clearTags request@ThreadRequest{..} =
  ApplyState (\st->st{_m_threadRequest = Just $! request{threadRequestTags = []}})
