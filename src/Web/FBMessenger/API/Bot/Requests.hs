{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- | This module contains data objects which represents requests to Messenger Platform Bot API
module Web.FBMessenger.API.Bot.Requests 
    ( -- * Types
      Recipient (..)
    , TextMessage (..)
    , NotificationType (..)
    , SendTextMessageRequest (..)
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

-- TODO: use message.attachment for StructuredMessages 

-- | This object represents the 'messages' request
data SendTextMessageRequest = SendTextMessageRequest
  { message_recipient         :: Recipient
  , message_message           :: TextMessage
  , message_notification_type :: Maybe NotificationType
  } deriving (Show, Generic)

instance ToJSON SendTextMessageRequest where
  toJSON = toJsonDrop 8

instance FromJSON SendTextMessageRequest where
  parseJSON = parseJsonDrop 8

-- | Recipient type for messages
data Recipient = Recipient 
  { recipient_phone_number :: Maybe Text
  , recipient_id :: Maybe Text
  } deriving (Show, Generic)
  -- NOTE: they cannot both be Nothing! Maybe we should rethink the structure...

instance ToJSON Recipient where
    toJSON = toJsonDrop 10

instance FromJSON Recipient where
    parseJSON = parseJsonDrop 10

-- | TextMessage text for simple text messages
-- NOTE: text must be UTF-8 and has a 320 character limit
data TextMessage = TextMessage { text_message_text :: Text } deriving (Show, Generic)

instance ToJSON TextMessage where
    toJSON = toJsonDrop 13

instance FromJSON TextMessage where
    parseJSON = parseJsonDrop 13

-- | NotificationType for messages
data NotificationType = Regular
                      | SilentPush
                      | NoPush
                      deriving Show

instance ToJSON NotificationType where
  toJSON Regular    = "REGULAR"
  toJSON SilentPush = "SILENT_PUSH"
  toJSON NoPush     = "NO_PUSH"

instance FromJSON NotificationType where
  parseJSON "REGULAR"     = pure Regular
  parseJSON "SILENT_PUSH" = pure SilentPush
  parseJSON "NO_PUSH"     = pure NoPush
  parseJSON _             = fail "Failed to parse NotificationType"
