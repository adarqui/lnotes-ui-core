{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Core.App.ThreadPost (
    setTitle
  , clearTitle
  , setBody
  , clearBody
  , setTag
  , addTag
  , deleteTag
  , clearTags
  , setPrivateTag
  , addPrivateTag
  , deletePrivateTag
  , clearPrivateTags
  , quote
) where



import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Text.Printf

import           LN.Generate.Default         (defaultThreadPostRequest)
import           LN.T
import qualified LN.UI.Core.App.Tag          as Tag
import           LN.UI.Core.Helpers.DataTime (forumUTCTimeMaybe)
import           LN.UI.Core.State



setTitle :: ThreadPostRequest -> Text -> Action
setTitle !request@ThreadPostRequest{..} !input =
  ApplyState (\st->st{_m_threadPostRequest = Just $ request{threadPostRequestTitle = Just input}})



clearTitle :: ThreadPostRequest -> Action
clearTitle !request@ThreadPostRequest{..} =
  ApplyState (\st->st{_m_threadPostRequest = Just $ request{threadPostRequestTitle = Nothing}})



setBody :: ThreadPostRequest -> PostData -> Action
setBody !request@ThreadPostRequest{..} !input =
  ApplyState (\st->st{_m_threadPostRequest = Just $ request{threadPostRequestBody = input}})



clearBody :: Action
clearBody =
  ApplyState (\st->st{_m_threadPostRequest = Nothing})



setTag :: ThreadPostRequest -> Text -> Action
setTag !request@ThreadPostRequest{..} !input =
   ApplyState (\st->
     st{
       _m_threadPostRequest = Just $ request{threadPostRequestStateTag = Just input}
     })



addTag :: ThreadPostRequest -> Action
addTag !request@ThreadPostRequest{..} =
  ApplyState (\st->
    st{
      _m_threadPostRequest = Just $ request{threadPostRequestTags = tags, threadPostRequestStateTag = Nothing}
    })
  where
  (tags, _) = Tag.addTag threadPostRequestTags threadPostRequestStateTag



deleteTag :: ThreadPostRequest -> Int -> Action
deleteTag !request@ThreadPostRequest{..} !idx =
  ApplyState (\st->
    st{
      _m_threadPostRequest = Just $ request{threadPostRequestTags = tags}
    })
  where
  tags = Tag.deleteTag threadPostRequestTags idx



clearTags :: ThreadPostRequest -> Action
clearTags !request@ThreadPostRequest{..} =
  ApplyState (\st->st{_m_threadPostRequest = Just $ request{threadPostRequestTags = []}})





setPrivateTag :: ThreadPostRequest -> Text -> Action
setPrivateTag !request@ThreadPostRequest{..} !input =
   ApplyState (\st->
     st{
       _m_threadPostRequest = Just $ request{threadPostRequestStatePrivateTag = Just input}
     })



addPrivateTag :: ThreadPostRequest -> Action
addPrivateTag !request@ThreadPostRequest{..} =
  ApplyState (\st->
    st{
      _m_threadPostRequest = Just $ request{threadPostRequestPrivateTags = tags, threadPostRequestStatePrivateTag = Nothing}
    })
  where
  (tags, _) = Tag.addTag threadPostRequestPrivateTags threadPostRequestStatePrivateTag



deletePrivateTag :: ThreadPostRequest -> Int -> Action
deletePrivateTag !request@ThreadPostRequest{..} !idx =
  ApplyState (\st->
    st{
      _m_threadPostRequest = Just $ request{threadPostRequestPrivateTags = tags}
    })
  where
  tags = Tag.deleteTag threadPostRequestPrivateTags idx



clearPrivateTags :: ThreadPostRequest -> Action
clearPrivateTags !request@ThreadPostRequest{..} =
  ApplyState (\st->st{_m_threadPostRequest = Just $ request{threadPostRequestPrivateTags = []}})



-- TODO FIXME - proper quote bbcode to text
-- ie, no raw "[quote] strings"
--
quote :: ThreadPostPackResponse -> Action
quote !response@ThreadPostPackResponse{..} =
  ApplyState (\st@Store{..} ->
    let
      request =
        case _m_threadPostRequest of
          Nothing ->
            case threadPostResponseBody of
              PostDataBBCode quote -> defaultThreadPostRequest { threadPostRequestBody = PostDataBBCode quoted_post }
              _                    -> defaultThreadPostRequest { threadPostRequestBody = PostDataEmpty }
          Just req@ThreadPostRequest{..} ->
            case (threadPostRequestBody, threadPostResponseBody) of
              (PostDataRaw ours, PostDataRaw quote)       -> req { threadPostRequestBody = PostDataRaw $ ours <> quote }
              (PostDataBBCode ours, PostDataBBCode quote) -> req { threadPostRequestBody = PostDataBBCode $ ours <> quoted_post }
              (_, PostDataBBCode quote)                   -> req { threadPostRequestBody = PostDataBBCode $ quoted_post }
              _                                           -> req { threadPostRequestBody = PostDataEmpty }
    in
      st{ _m_threadPostRequest = Just request })
  where
  ThreadPostResponse{..}    = threadPostPackResponseThreadPost
  UserSanitizedResponse{..} = threadPostPackResponseUser
  quoted_post =
    case threadPostResponseBody of
      PostDataBBCode bbcode -> Text.pack $
        printf "[quote author_name=adarqui author=%d link=/translate/thread_post/%d date=%s id=%d]\n%s\n[/quote id=%d]"
          userSanitizedResponseId
          threadPostResponseId
          (forumUTCTimeMaybe threadPostResponseCreatedAt)
          threadPostResponseId
          bbcode
          threadPostResponseId
        -- printf "[quote author=%s link= date= post_id=%d author_uid=%d]%s[/quote post_id=%d]"
        --  userSanitizedResponseDisplayName threadPostResponseId userSanitizedResponseId bbcode threadPostResponseId
      _                     -> "LN.UI.Core.App.ThreadPost: quote problem."
