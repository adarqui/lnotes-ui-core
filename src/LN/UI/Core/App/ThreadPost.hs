{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module LN.UI.Core.App.ThreadPost (
    setTitle
  , clearTitle
  , setBody
  , setTag
  , addTag
  , deleteTag
  , clearTags
  , setPrivateTag
  , addPrivateTag
  , deletePrivateTag
  , clearPrivateTags
) where



import           Data.Text          (Text)

import           LN.T
import qualified LN.UI.Core.App.Tag as Tag
import           LN.UI.Core.State



setTitle :: ThreadPostRequest -> Text -> Action
setTitle request@ThreadPostRequest{..} input =
  ApplyState (\st->st{_m_threadPostRequest = Just $! request{threadPostRequestTitle = Just input}})



clearTitle :: ThreadPostRequest -> Action
clearTitle request@ThreadPostRequest{..} =
  ApplyState (\st->st{_m_threadPostRequest = Just $! request{threadPostRequestTitle = Nothing}})



setBody :: ThreadPostRequest -> PostData -> Action
setBody request@ThreadPostRequest{..} input =
  ApplyState (\st->st{_m_threadPostRequest = Just $! request{threadPostRequestBody = input}})



setTag :: ThreadPostRequest -> Text -> Action
setTag request@ThreadPostRequest{..} input =
   ApplyState (\st->
     st{
       _m_threadPostRequest = Just $! request{threadPostRequestStateTag = Just input}
     })



addTag :: ThreadPostRequest -> Action
addTag request@ThreadPostRequest{..} =
  ApplyState (\st->
    st{
      _m_threadPostRequest = Just $! request{threadPostRequestTags = tags, threadPostRequestStateTag = Nothing}
    })
  where
  (tags, _) = Tag.addTag threadPostRequestTags threadPostRequestStateTag



deleteTag :: ThreadPostRequest -> Int -> Action
deleteTag request@ThreadPostRequest{..} idx =
  ApplyState (\st->
    st{
      _m_threadPostRequest = Just $! request{threadPostRequestTags = tags}
    })
  where
  tags = Tag.deleteTag threadPostRequestTags idx



clearTags :: ThreadPostRequest -> Action
clearTags request@ThreadPostRequest{..} =
  ApplyState (\st->st{_m_threadPostRequest = Just $! request{threadPostRequestTags = []}})





setPrivateTag :: ThreadPostRequest -> Text -> Action
setPrivateTag request@ThreadPostRequest{..} input =
   ApplyState (\st->
     st{
       _m_threadPostRequest = Just $! request{threadPostRequestStatePrivateTag = Just input}
     })



addPrivateTag :: ThreadPostRequest -> Action
addPrivateTag request@ThreadPostRequest{..} =
  ApplyState (\st->
    st{
      _m_threadPostRequest = Just $! request{threadPostRequestPrivateTags = tags, threadPostRequestStatePrivateTag = Nothing}
    })
  where
  (tags, _) = Tag.addTag threadPostRequestPrivateTags threadPostRequestStatePrivateTag



deletePrivateTag :: ThreadPostRequest -> Int -> Action
deletePrivateTag request@ThreadPostRequest{..} idx =
  ApplyState (\st->
    st{
      _m_threadPostRequest = Just $! request{threadPostRequestPrivateTags = tags}
    })
  where
  tags = Tag.deleteTag threadPostRequestPrivateTags idx



clearPrivateTags :: ThreadPostRequest -> Action
clearPrivateTags request@ThreadPostRequest{..} =
  ApplyState (\st->st{_m_threadPostRequest = Just $! request{threadPostRequestPrivateTags = []}})
