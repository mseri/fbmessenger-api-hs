{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- | This module contains responses from Messenger Platform Bot API
module Web.FBMessenger.API.Bot.Responses (
    SubscriptionResponse (..)
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
