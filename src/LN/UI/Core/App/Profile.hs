{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE RecordWildCards #-}

module LN.UI.Core.App.Profile (
    setGender
  , setBirthdate
  , setWebsite
  , clearWebsite
  , setWebsites
  , clearWebsites
  , setLocation
  , clearLocation
  , setSignature
  , clearSignature
  , setDebug
) where



import           Data.Text        (Text)
import           Data.Time        (UTCTime)

import           LN.T
import           LN.UI.Core.State



setGender :: ProfileRequest -> ProfileGender -> Action
setGender !request@ProfileRequest{..} !input =
  ApplyState (\st->st{_m_profileRequest = Just $ request{profileRequestGender = input}})



setBirthdate :: ProfileRequest -> UTCTime -> Action
setBirthdate _ _ = undefined



setWebsite :: ProfileRequest -> Text -> Action
setWebsite !request@ProfileRequest{..} !input =
  ApplyState (\st->st{_m_profileRequest = Just $ request{profileRequestWebsite = Just input}})



clearWebsite :: ProfileRequest -> Action
clearWebsite !request@ProfileRequest{..} =
  ApplyState (\st->st{_m_profileRequest = Just $ request{profileRequestWebsite = Nothing}})



setWebsites :: ProfileRequest -> [Text] -> Action
setWebsites !request@ProfileRequest{..} !input =
  ApplyState (\st->st{_m_profileRequest = Just $ request{profileRequestWebsites = input}})



clearWebsites :: ProfileRequest -> Action
clearWebsites !request@ProfileRequest{..} =
  ApplyState (\st->st{_m_profileRequest = Just $ request{profileRequestWebsites = []}})



setLocation :: ProfileRequest -> Text -> Action
setLocation !request@ProfileRequest{..} !input =
  ApplyState (\st->st{_m_profileRequest = Just $ request{profileRequestLocation = Just input}})



clearLocation :: ProfileRequest -> Action
clearLocation !request@ProfileRequest{..} =
  ApplyState (\st->st{_m_profileRequest = Just $ request{profileRequestLocation = Nothing}})



setSignature :: ProfileRequest -> Text -> Action
setSignature !request@ProfileRequest{..} !input =
  ApplyState (\st->st{_m_profileRequest = Just $ request{profileRequestSignature = Just input}})



clearSignature :: ProfileRequest -> Action
clearSignature !request@ProfileRequest{..} =
  ApplyState (\st->st{_m_profileRequest = Just $ request{profileRequestSignature = Nothing}})



setDebug :: ProfileRequest -> Bool -> Action
setDebug !request@ProfileRequest{..} !input =
  ApplyState (\st->st{_m_profileRequest = Just $ request{profileRequestDebug = input}})
