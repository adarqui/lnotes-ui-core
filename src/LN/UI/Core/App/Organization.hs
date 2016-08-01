{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module LN.UI.Core.App.Organization (
    setDisplayName
  , setDescription
  , clearDescription
  , setCompany
  , setLocation
  , setMembership
  , setVisibility
  , setTag
  , addTag
  , deleteTag
  , clearTags
) where



import           Data.Text                   (Text)

import           LN.T
import qualified LN.UI.Core.App.Tag          as Tag
import           LN.UI.Core.State



setDisplayName :: OrganizationRequest -> Text -> Action
setDisplayName request@OrganizationRequest{..} input =
  ApplyState (\st->st{_m_organizationRequest = Just $! request{organizationRequestDisplayName = input}})



setDescription :: OrganizationRequest -> Text -> Action
setDescription request@OrganizationRequest{..} input =
  ApplyState (\st->st{_m_organizationRequest = Just $! request{organizationRequestDescription = Just $! input}})



clearDescription :: OrganizationRequest -> Action
clearDescription request@OrganizationRequest{..} =
  ApplyState (\st->st{_m_organizationRequest = Just $! request{organizationRequestDescription = Nothing }})



setCompany :: OrganizationRequest -> Text -> Action
setCompany request@OrganizationRequest{..} input =
  ApplyState (\st->st{_m_organizationRequest = Just $! request{organizationRequestCompany = input}})



setLocation :: OrganizationRequest -> Text -> Action
setLocation request@OrganizationRequest{..} input =
  ApplyState (\st->st{_m_organizationRequest = Just $! request{organizationRequestLocation = input}})



setMembership :: OrganizationRequest -> Membership -> Action
setMembership request@OrganizationRequest{..} input =
  ApplyState (\st->st{_m_organizationRequest = Just $! request{organizationRequestMembership = input}})



setVisibility :: OrganizationRequest -> Visibility -> Action
setVisibility request@OrganizationRequest{..} input =
  ApplyState (\st->st{_m_organizationRequest = Just $! request{organizationRequestVisibility = input}})



setTag :: OrganizationRequest -> Text -> Action
setTag request@OrganizationRequest{..} input =
   ApplyState (\st->
     st{
       _m_organizationRequest = Just $! request{organizationRequestStateTag = Just input}
     })


addTag :: OrganizationRequest -> Maybe Text -> Action
addTag request@OrganizationRequest{..} m_tag =
  ApplyState (\st->
    st{
      _m_organizationRequest = Just $! request{organizationRequestTags = tags, organizationRequestStateTag = Nothing}
    })
  where
  (tags, _) = Tag.addTag organizationRequestTags m_tag



deleteTag :: OrganizationRequest -> Int -> Action
deleteTag request@OrganizationRequest{..} idx =
  ApplyState (\st->
    st{
      _m_organizationRequest = Just $! request{organizationRequestTags = tags}
    })
  where
  tags = Tag.deleteTag organizationRequestTags idx



clearTags :: OrganizationRequest -> Action
clearTags request@OrganizationRequest{..} =
  ApplyState (\st->st{_m_organizationRequest = Just $! request{organizationRequestTags = []}})
