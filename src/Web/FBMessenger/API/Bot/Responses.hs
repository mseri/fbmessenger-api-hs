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
  , message_response_message_id   :: Text
  } deriving (Show, Generic)

instance ToJSON MessageResponse where
  toJSON = toJsonDrop 17

instance FromJSON MessageResponse where
  parseJSON = parseJsonDrop 17


-- | This objects contains the User Profile informations
data UserProfileResponse = UserProfileResponse 
  { usr_first_name  :: Text
  , usr_last_name   :: Text
  , usr_profile_pic :: Text
  , usr_locale      :: Text
  , usr_timezone    :: Int
  , usr_gender      :: Text 
  } deriving (Show, Generic)
  
instance ToJSON UserProfileResponse where
  toJSON = toJsonDrop 4
  
instance FromJSON UserProfileResponse where
  parseJSON = parseJsonDrop 4


-- | This objects contains informations on the succesful setup of a welcome message
data WelcomeMessageResponse = WelcomeMessageResponse { wmr_result :: Text } deriving (Show, Generic)

instance ToJSON WelcomeMessageResponse where
  toJSON = toJsonDrop 4

instance FromJSON WelcomeMessageResponse where
  parseJSON = parseJsonDrop 4


-- TODO: add errors linter: https://developers.facebook.com/docs/messenger-platform/send-api-reference#errors
-- Code	Description
-- 2     Send message failure. Internal server error.
-- 10    Application does not have permission to use the Send API
-- 100   No matching user found
-- 613   Calls to this api have exceeded the rate limit.
