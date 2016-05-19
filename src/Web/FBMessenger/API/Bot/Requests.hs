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
    , SendStructuredMessageRequest (..)
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


-- | Informations about the recipient of the message
data Recipient = Recipient 
  { recipient_phone_number :: Maybe Text  -- Phone number of the recipient with the format +1(212)555-2368
  , recipient_id           :: Maybe Text  -- ID of recipient
  } deriving (Show, Generic)
  -- WARN: they cannot both be Nothing! Maybe we should rethink the structure...

instance ToJSON Recipient where
    toJSON = toJsonDrop 10

instance FromJSON Recipient where
    parseJSON = parseJsonDrop 10


-- | Content of the message for a text-only message
data TextMessage = TextMessage 
  { text_message_text :: Text       -- Message text, must be UTF-8, 320 character limit 
  } deriving (Show, Generic)

instance ToJSON TextMessage where
    toJSON = toJsonDrop 13

instance FromJSON TextMessage where
    parseJSON = parseJsonDrop 13


-- | Push notification type for the message
data NotificationType = Regular        -- will emit a sound/vibration and a phone notification (default)
                      | SilentPush     -- will just emit a phone notification
                      | NoPush         -- will not emit either
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


-- | This object represents a text message request
data SendTextMessageRequest = SendTextMessageRequest
  { message_recipient         :: Recipient
  , message_message           :: TextMessage
  , message_notification_type :: Maybe NotificationType
  } deriving (Show, Generic)

instance ToJSON SendTextMessageRequest where
  toJSON = toJsonDrop 8

instance FromJSON SendTextMessageRequest where
  parseJSON = parseJsonDrop 8


-- TODO: use message.attachment for StructuredMessages 
--       see https://developers.facebook.com/docs/messenger-platform/send-api-reference#request
-- Consider to reimplement separating by hand all the various possible requests (image, and the three templates)

-- | Type of attachment for a structured message
data AttachmentType = AttachmentImage 
                    | AttachmentTemplate 
                    deriving Show

instance ToJSON AttachmentType where
  toJSON AttachmentImage    = "image"
  toJSON AttachmentTemplate = "template"

instance FromJSON AttachmentType where
  parseJSON "image"    = pure AttachmentImage
  parseJSON "template" = pure AttachmentTemplate
  parseJSON _          = fail "Failed to parse AttachmentType"


-- | Attachment for a structured message
data MessageAttachment = MessageAttachment
  { message_attachment_type    :: AttachmentType
  , message_attachment_payload :: ImagePayload -- AttachmentPayload
  } deriving (Show, Generic)
  -- WARN: the payload depends on the type!! Maybe we should rethink the structure...

instance ToJSON MessageAttachment where
    toJSON = toJsonDrop 19

instance FromJSON MessageAttachment where
    parseJSON = parseJsonDrop 19

-- | Payload of attachment for a structured message
data ImagePayload = ImagePayload { image_url :: Text } deriving (Show, Generic)

instance ToJSON ImagePayload where
    toJSON = toJsonDrop 6

instance FromJSON ImagePayload where
    parseJSON = parseJsonDrop 6
    
-- TODO: add the following payloads
-- data AttachmentPayload = ImagePayload 
-- | GenericTemplate 
--   { generict_template_type  :: TemplateType      -- Value must be "generic"
--   , genetict_elements       :: [Element]         -- Data for each bubble in message 
--   }
-- | ButtonTemplate  
--   { buttontp_template_type  :: TemplateType      -- Value must be "button"
--   , buttontp_text           :: Text              -- Text that appears in main body
--   , buttontp_buttons        :: [Button]          -- Set of buttons that appear as call-to-actions
--   }
-- | ReceiptTemplate 
--   { receiptt_template_type  :: TemplateType      -- Value should be "receipt"
--   , receiptt_recipient_name :: Text              -- Recipient's Name
--   , receiptt_order_number   :: Text              -- Order number. Must be unique
--   , receiptt_currency       :: Text              -- Currency for order
--   , receiptt_payment_method :: Text              -- Payment method details. This can be a custom string. Ex: 'Visa 1234'
--   , receiptt_timestamp      :: Maybe Text        -- Timestamp of order
--   , receiptt_order_url      :: Maybe Text        -- URL of order
--   , receiptt_elements       :: [ReceiptElements] -- Items in order       
--   , receiptt_address        :: Maybe ShippingAddress -- Shipping address
--   , receiptt_summary        :: PaymentSummary    -- Payment summary
--   , receiptt_adjustment     :: PaymentAdjustments -- Payment adjustments
--   }     

-- | Content of the message for a structured message
data StructuredMessage = StructuredMessage 
  { structured_message_attachment :: MessageAttachment       -- Message text, must be UTF-8, 320 character limit 
  } deriving (Show, Generic)

instance ToJSON StructuredMessage where
    toJSON = toJsonDrop 19

instance FromJSON StructuredMessage where
    parseJSON = parseJsonDrop 19


-- | This object represents a structured message request
data SendStructuredMessageRequest = SendStructuredMessageRequest
  { structured_message_recipient         :: Recipient
  , structured_message_message           :: StructuredMessage         
  , structured_message_notification_type :: Maybe NotificationType
  } deriving (Show, Generic)

instance ToJSON SendStructuredMessageRequest where
  toJSON = toJsonDrop 19

instance FromJSON SendStructuredMessageRequest where
  parseJSON = parseJsonDrop 19