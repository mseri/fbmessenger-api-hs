{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Web.FBMessenger.API.Bot.WebhookRequests (
    
) where
    -- Framp, this is for you :)
import           Control.Monad (when)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Lazy (member)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           GHC.Generics
import           Web.FBMessenger.API.Bot.Requests
import           Web.FBMessenger.API.Bot.JsonExt


data WRRequest = WRRequest{ wrrEntries :: [WREvent] } deriving Show
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
    } deriving (Show, Generic)

instance ToJSON WREvent where
    toJSON = toJsonDrop 5
instance FromJSON WREvent where
    parseJSON = parseJsonDrop 5


data WRMessage = WRMessage 
    { wrmSenderId    :: Text
    , wrmRecipientId :: Text
    , wrmTimestamp   :: Int
    , wrmContent     :: WRMessageContent
    } deriving Show

instance ToJSON WRMessage where 
    toJSON WRMessage{..} = 
        let content = case wrmContent of 
                        WRMTextMessage       -> "message"
                        WRMStructuredMessage -> "message"
                        WRMAuth              -> "optin"
                        WRMDelivery          -> "delivery"
                        WRMPostback          -> "postback"
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
                  let wrmContent = case head wrmTypeChoices of
                                      "message"  -> WRMTextMessage -- we will have to check if "attachment" is present and discern the messages
                                      "optin"    -> WRMAuth
                                      "delivery" -> WRMDelivery
                                      "postback" -> WRMPostback
                                      _          -> error "this cannot happen by construction, but I want to make the compiler happy"
                  return WRMessage{..}

-- TODO: properly define the message content and its serialization/deserialization

data WRMessageContent = WRMTextMessage | WRMStructuredMessage | WRMAuth | WRMDelivery | WRMPostback deriving (Show, Generic)
instance ToJSON WRMessageContent
instance FromJSON WRMessageContent
