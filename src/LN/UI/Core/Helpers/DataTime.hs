{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Core.Helpers.DataTime (
    prettyUTCTime
  , prettyUTCTimeMaybe
  , forumUTCTime
  , forumUTCTimeMaybe
) where



import           Data.Text (Text)
import qualified Data.Text as Text (pack)
import           Data.Time



-- | Evaluates time in this format:
--
--
prettyUTCTime :: UTCTime -> Text
prettyUTCTime utc_time =
  Text.pack $ formatTime defaultTimeLocale "%B %Y" utc_time



-- | Evaluates Maybe time in this format:
--
--
prettyUTCTimeMaybe :: Maybe UTCTime -> Text
prettyUTCTimeMaybe Nothing = "No time."
prettyUTCTimeMaybe (Just utc_time) = prettyUTCTime utc_time



-- | Returns time in this format:
--
--
forumUTCTime :: UTCTime -> Text
forumUTCTime utc_time =
  Text.pack $ formatTime defaultTimeLocale "%s" utc_time



-- | Evaluates maybe time in this format:
--
--
forumUTCTimeMaybe :: Maybe UTCTime -> Text
forumUTCTimeMaybe Nothing = "0"
forumUTCTimeMaybe (Just utc_time) = forumUTCTime utc_time
