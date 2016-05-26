{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Web.FBMessenger.API.Bot.WebhookRequests (
    
) where
    
import           Control.Monad (when)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Lazy (member)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           GHC.Generics
import           Web.FBMessenger.API.Bot.Requests
import           Web.FBMessenger.API.Bot.JsonExt

-- TODO: add docstring, simplify api and representation
-- Framp, this is for you :)

data WRRequest = WRRequest{ wrrEntries :: [WREvent] } deriving (Show, Eq)
instance ToJSON WRRequest where 
    toJSON WRRequest{..} = object [ "object" .= ("page"::String), "entry" .= wrrEntries ]
instance FromJSON WRRequest where
    parseJSON = withObject "WebSocket request" $ \o -> do
                  obj <- o .: "object"
                  when (obj /= ("page"::String)) $ 
                    fail "invalid messaging event request"
                  wrrEntries <- o .: "entry"
                  return WRRequest{..}

data WREvent = WREvent 
    { page_id        :: Int           -- ^ Page ID of page 
    , page_time      :: Int           -- ^ Time of update
    , page_messaging :: [WRMessage]   -- ^ Array containing objects related to messaging
    } deriving (Show, Eq, Generic)

instance ToJSON WREvent where
    toJSON = toJsonDrop 5
instance FromJSON WREvent where
    parseJSON = parseJsonDrop 5


data WRMessage = WRMessage 
    { wrmSenderId    :: Text
    , wrmRecipientId :: Text
    , wrmTimestamp   :: Int
    , wrmContent     :: WRMessageContent
    } deriving (Show, Eq)

instance ToJSON WRMessage where 
    toJSON WRMessage{..} = 
        let content = case wrmContent of 
                        WRMTextMessage{}       -> "message"
                        WRMStructuredMessage{} -> "message"
                        WRMAuth{}              -> "optin"
                        WRMDelivery{}          -> "delivery"
                        WRMPostback{}          -> "postback"
        in object [ "sender"    .= object [ "id" .= wrmSenderId ]
                  , "recipient" .= object [ "id" .= wrmRecipientId ] 
                  , "timestamp" .= wrmTimestamp
                  , content     .= wrmContent ]
                            
instance FromJSON WRMessage where
    parseJSON = withObject "WebSocket message content" $ \o -> do
                  wrmSenderId    <- o .: "sender" >>= (.: "id")
                  wrmRecipientId <- o .: "recipient" >>= (.: "id")
                  wrmTimestamp   <- o .: "timestamp"
                  -- not too clean but seems to do the job
                  -- if we refactor, it's faster if we get the first true only
                  let wrmTypeChoices = filter (`member` o) (["message", "optin", "delivery", "postback"]::[Text])
                  when (null wrmTypeChoices) $ 
                    fail "unknown message content"
                  -- here I am assuming only one kind of content per request
                  -- wrmContent <- case head wrmTypeChoices of
                  --                     "message"  -> (o .: "message")
                  --                     "optin"    -> (o .: "optin")
                  --                     "delivery" -> (o .: "delivery")
                  --                     "postback" -> (o .: "postback")
                  --                     _          -> error "this cannot happen by construction, but I want to make the compiler happy"
                  wrmContent <- o .: head wrmTypeChoices 
                  return WRMessage{..}

data WRMessageContent = WRMTextMessage Text Int Text       -- ^ Message ID; Message sequence number; Message text. 
                      | WRMStructuredMessage Text Int [WRMessageAttachment] -- ^ Message ID; Message sequence number; Array containing attachment data (image, video, audio)
                      | WRMAuth Text                       -- ^ data-ref parameter that was defined with the entry point
                      | WRMDelivery Int Int (Maybe [Text]) -- ^ Sequence No.; Watermark: all messages that were sent before this timestamp were delivered; Array containing message IDs of messages that were delivered (optional) 
                      | WRMPostback Text                   -- ^ Contains the postback payload that was defined with the button
                      deriving (Show, Eq)
instance ToJSON WRMessageContent where
    toJSON (WRMTextMessage mid seq text) = object [ "mid" .= mid, "seq" .= seq, "text" .= text ] 
    toJSON (WRMStructuredMessage mid seq attachments) = object [ "mid" .= mid, "seq" .= seq, "attachments" .= attachments ]
    toJSON (WRMAuth ref) = object [ "ref" .= ref ]
    toJSON (WRMDelivery seq watermark mids) = omitNulls [ "seq" .= seq, "watermark" .= watermark, "mids" .= mids ]
    toJSON (WRMPostback payload) = object [ "payload" .= payload ]
    
instance FromJSON WRMessageContent where
    parseJSON = withObject "message content" $ \o -> do
        let typeChoices = filter (`member` o) (["text", "attachments", "ref", "watermark", "payload"]::[Text])
        when (null typeChoices) $ 
            fail "unknown message content"
        case head typeChoices of
            "text"        -> WRMTextMessage <$> o .: "mid" <*> o .: "seq" <*> o .: "text"
            "attachments" -> WRMStructuredMessage <$> o .: "mid"  <*> o .: "seq" <*> o .: "attachments"
            "ref"         -> WRMAuth <$> o .: "ref"
            "watermark"   -> WRMDelivery <$> o .: "seq" <*> o .: "watermark" <*> o .:? "mids"
            "payload"     -> WRMPostback <$> o .: "payload"
            _             -> error "this cannot happen by construction, but I want to make the compiler happy"


data WRMessageAttachment = WRMessageAttachment { wrmaType :: WRAttachmentType, wrmaUrl :: Text } deriving (Show, Eq)
instance ToJSON WRMessageAttachment where
    toJSON WRMessageAttachment{..} = object [ "type" .= wrmaType, "payload" .= object [ "url" .= wrmaUrl ] ]
instance FromJSON WRMessageAttachment where
    parseJSON = withObject "websocket call message attachment" $ \o -> do
        wrmaType <- o .: "type"
        wrmaUrl  <- o .: "payload" >>= (.: "url")
        return WRMessageAttachment{..} 
    
data WRAttachmentType = ATImage | ATVideo | ATAudio deriving (Show, Eq)
instance ToJSON WRAttachmentType where
    toJSON ATImage = "image"
    toJSON ATVideo = "video"
    toJSON ATAudio = "audio" 
instance FromJSON WRAttachmentType where
    parseJSON "image" = pure ATImage
    parseJSON "video" = pure ATVideo
    parseJSON "audio" = pure ATAudio
    parseJSON _       = fail "impossible to parse AttachmentType"
    


-- Helpers

-- from http://bitemyapp.com/posts/2014-07-31-aeson-with-uncertainty-revised.html
omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull where
  notNull (_, Null) = False
  notNull _         = True
