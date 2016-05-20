{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- | This module contains responses from Messenger Platform Bot API
module Web.FBMessenger.API.Bot.Responses 
  ( -- * Types 
    MessageResponse (..)
  , SubscriptionResponse (..)
  , UserProfileResponse
  , WelcomeMessageResponse
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           Web.FBMessenger.API.Bot.JsonExt

-- | This object represents the 'subscribed_apps' response
data SubscriptionResponse = SubscriptionResponse
  {
    subscription_success :: Bool
  } deriving (Show, Generic)

instance ToJSON SubscriptionResponse where
  toJSON = toJsonDrop 13

instance FromJSON SubscriptionResponse where
  parseJSON = parseJsonDrop 13

-- | This object contais the response after a message has been succesfully sent 
data MessageResponse = MessageResponse 
  { message_response_recipient_id :: Text
  , message_response_message_id :: Text
  } deriving (Show, Generic)

instance ToJSON MessageResponse where
  toJSON = toJsonDrop 17

instance FromJSON MessageResponse where
  parseJSON = parseJsonDrop 17

-- TODO: implement those
data UserProfileResponse = UserProfileResponse {} deriving (Show, Generic)
instance ToJSON UserProfileResponse
instance FromJSON UserProfileResponse

data WelcomeMessageResponse = WelcomeMessageResponse {} deriving (Show, Generic)
instance ToJSON WelcomeMessageResponse
instance FromJSON WelcomeMessageResponse

-- TODO: add errors linter: https://developers.facebook.com/docs/messenger-platform/send-api-reference#errors
