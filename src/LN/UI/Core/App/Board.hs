{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module LN.UI.Core.App.Board (
    setDisplayName
  , setDescription
  , clearDescription
  , setTag
  , addTag
  , deleteTag
  , clearTags
  , setSuggestedTag
  , addSuggestedTag
  , deleteSuggestedTag
  , clearSuggestedTags
) where



import           Data.Text          (Text)

import           LN.T
import qualified LN.UI.Core.App.Tag as Tag
import           LN.UI.Core.State



setDisplayName :: BoardRequest -> Text -> Action
setDisplayName request@BoardRequest{..} input =
  ApplyState (\st->st{_m_boardRequest = Just $! request{boardRequestDisplayName = input}})



setDescription :: BoardRequest -> Text -> Action
setDescription request@BoardRequest{..} input =
  ApplyState (\st->st{_m_boardRequest = Just $! request{boardRequestDescription = Just $! input}})



clearDescription :: BoardRequest -> Action
clearDescription request@BoardRequest{..} =
  ApplyState (\st->st{_m_boardRequest = Just $! request{boardRequestDescription = Nothing }})



-- setVisibility :: BoardRequest -> Visibility -> Action
-- setVisibility request@BoardRequest{..} input =
--   ApplyState (\st->st{_m_boardRequest = Just $! request{boardRequestVisibility = input}})



setTag :: BoardRequest -> Text -> Action
setTag request@BoardRequest{..} input =
   ApplyState (\st->
     st{
       _m_boardRequest = Just $! request{boardRequestStateTag = Just input}
     })



addTag :: BoardRequest -> Action
addTag request@BoardRequest{..} =
  ApplyState (\st->
    st{
      _m_boardRequest = Just $! request{boardRequestTags = tags, boardRequestStateTag = Nothing}
    })
  where
  (tags, _) = Tag.addTag boardRequestTags boardRequestStateTag



deleteTag :: BoardRequest -> Int -> Action
deleteTag request@BoardRequest{..} idx =
  ApplyState (\st->
    st{
      _m_boardRequest = Just $! request{boardRequestTags = tags}
    })
  where
  tags = Tag.deleteTag boardRequestTags idx



clearTags :: BoardRequest -> Action
clearTags request@BoardRequest{..} =
  ApplyState (\st->st{_m_boardRequest = Just $! request{boardRequestTags = []}})



setSuggestedTag :: BoardRequest -> Text -> Action
setSuggestedTag request@BoardRequest{..} input =
   ApplyState (\st->
     st{
       _m_boardRequest = Just $! request{boardRequestStateSuggestedTag = Just input}
     })



addSuggestedTag :: BoardRequest -> Action
addSuggestedTag request@BoardRequest{..} =
  ApplyState (\st->
    st{
      _m_boardRequest = Just $! request{boardRequestSuggestedTags = tags, boardRequestStateSuggestedTag = Nothing}
    })
  where
  (tags, _) = Tag.addTag boardRequestSuggestedTags boardRequestStateSuggestedTag



deleteSuggestedTag :: BoardRequest -> Int -> Action
deleteSuggestedTag request@BoardRequest{..} idx =
  ApplyState (\st->
    st{
      _m_boardRequest = Just $! request{boardRequestSuggestedTags = tags}
    })
  where
  tags = Tag.deleteTag boardRequestSuggestedTags idx



clearSuggestedTags :: BoardRequest -> Action
clearSuggestedTags request@BoardRequest{..} =
  ApplyState (\st->st{_m_boardRequest = Just $! request{boardRequestSuggestedTags = []}})
