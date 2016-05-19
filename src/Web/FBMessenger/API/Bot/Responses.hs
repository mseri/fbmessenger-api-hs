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
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           Web.FBMessenger.API.Bot.Data
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
-- TODO: understand how to implement the errors: https://developers.facebook.com/docs/messenger-platform/send-api-reference#errors
data MessageResponse = MessageResponse 
  { message_response_recipient_id :: Text
  , message_response_message_id :: Text
  } deriving (Show, Generic)

instance ToJSON MessageResponse where
  toJSON = toJsonDrop 17

instance FromJSON MessageResponse where
  parseJSON = parseJsonDrop 17
