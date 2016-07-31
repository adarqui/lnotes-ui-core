{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module LN.UI.Core.App.Forum (
    setDisplayName
  , setDescription
  , clearDescription
  , setVisibility
  , setThreadsPerBoard
  , setThreadPostsPerThread
  , setRecentThreadsLimit
  , setRecentPostsLimit
  , setMotwLimit
  , setTag
  , addTag
  , deleteTag
  , clearTags
) where



import           Data.Text          (Text)

import           LN.T
import qualified LN.UI.Core.App.Tag as Tag
import           LN.UI.Core.State



setDisplayName :: ForumRequest -> Text -> Action
setDisplayName request@ForumRequest{..} input =
  ApplyState (\st->st{_m_forumRequest = Just $! request{forumRequestDisplayName = input}})



setDescription :: ForumRequest -> Text -> Action
setDescription request@ForumRequest{..} input =
  ApplyState (\st->st{_m_forumRequest = Just $! request{forumRequestDescription = Just $! input}})



clearDescription :: ForumRequest -> Action
clearDescription request@ForumRequest{..} =
  ApplyState (\st->st{_m_forumRequest = Just $! request{forumRequestDescription = Nothing }})



setThreadsPerBoard :: ForumRequest -> Int -> Action
setThreadsPerBoard request@ForumRequest{..} input =
  ApplyState (\st->st{_m_forumRequest = Just $! request{forumRequestThreadsPerBoard = input}})



setThreadPostsPerThread :: ForumRequest -> Int -> Action
setThreadPostsPerThread request@ForumRequest{..} input =
  ApplyState (\st->st{_m_forumRequest = Just $! request{forumRequestThreadPostsPerThread = input}})



setRecentThreadsLimit :: ForumRequest -> Int -> Action
setRecentThreadsLimit request@ForumRequest{..} input =
  ApplyState (\st->st{_m_forumRequest = Just $! request{forumRequestRecentThreadsLimit = input}})



setRecentPostsLimit :: ForumRequest -> Int -> Action
setRecentPostsLimit request@ForumRequest{..} input =
  ApplyState (\st->st{_m_forumRequest = Just $! request{forumRequestRecentPostsLimit = input}})



setMotwLimit :: ForumRequest -> Int -> Action
setMotwLimit request@ForumRequest{..} input =
  ApplyState (\st->st{_m_forumRequest = Just $! request{forumRequestMotwLimit = input}})



setVisibility :: ForumRequest -> Visibility -> Action
setVisibility request@ForumRequest{..} input =
  ApplyState (\st->st{_m_forumRequest = Just $! request{forumRequestVisibility = input}})



setTag :: ForumRequest -> Text -> Action
setTag request@ForumRequest{..} input =
   ApplyState (\st->
     st{
       _m_forumRequest = Just $! request
     , _m_forumRequestTag = Just $! input
     })


addTag :: ForumRequest -> Maybe Text -> Action
addTag request@ForumRequest{..} m_tag =
  ApplyState (\st->
    st{
      _m_forumRequest = Just $! request{forumRequestTags = tags}
    , _m_forumRequestTag = Nothing
    })
  where
  (tags, _) = Tag.addTag forumRequestTags m_tag



deleteTag :: ForumRequest -> Int -> Action
deleteTag request@ForumRequest{..} idx =
  ApplyState (\st->
    st{
      _m_forumRequest = Just $! request{forumRequestTags = tags}
    })
  where
  tags = Tag.deleteTag forumRequestTags idx



clearTags :: ForumRequest -> Action
clearTags request@ForumRequest{..} =
  ApplyState (\st->st{_m_forumRequest = Just $! request{forumRequestTags = []}})
