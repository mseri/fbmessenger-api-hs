-- |
-- Module      : Web.FBMessenger.API.Bot.Requests 
-- License     : BSD3
-- Maintainer  : Marcello Seri <marcello.seri@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains types and helpers to parse the responses from the
-- <https://developers.facebook.com/docs/messenger-platform/ Messenger Platform API>. 
-- See also 'Web.FBMessenger.API.Bot.SendAPI'.
--
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  #-}

module Web.FBMessenger.API.Bot.Responses 
  ( -- * Types 
    MessageResponse        (..)
  , SendErrorCode          (..)
  , SendErrorObject        (..)
  , SubscriptionResponse   (..)
  , UserProfileResponse    (..)
  , WelcomeMessageResponse (..)
  -- * Functions
  , errorInfo
  , extractSendError
  ) where

import           Data.Aeson
import           Data.Text (Text)
import           GHC.Generics
import           Servant.Client (ServantError(..))
import           Web.FBMessenger.API.Bot.JsonExt


-- | This object represents the 'subscribed_apps' success response
data SubscriptionResponse = SubscriptionResponse{ subscription_success :: Bool } deriving (Eq, Show, Generic)

instance ToJSON SubscriptionResponse where
  toJSON = toJsonDrop 13

instance FromJSON SubscriptionResponse where
  parseJSON = parseJsonDrop 13

-- | This object contais the response after a message has been succesfully sent 
data MessageResponse = MessageResponse 
  { message_response_recipient_id :: Text
  , message_response_message_id   :: Text
  } deriving (Eq, Show, Generic)

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
  } deriving (Eq, Show, Generic)
  
instance ToJSON UserProfileResponse where
  toJSON = toJsonDrop 4
  
instance FromJSON UserProfileResponse where
  parseJSON = parseJsonDrop 4


-- | This objects contains informations on the succesful setup of a welcome message
data WelcomeMessageResponse = WelcomeMessageResponse { wmr_result :: Text } deriving (Eq, Show, Generic)

instance ToJSON WelcomeMessageResponse where
  toJSON = toJsonDrop 4

instance FromJSON WelcomeMessageResponse where
  parseJSON = parseJsonDrop 4

-- | Send API Error response object (see: https://developers.facebook.com/docs/messenger-platform/send-api-reference#errors)
-- 
--   {
--    "error":{
--       "message":"Invalid parameter",
--       "type":"FacebookApiException",
--       "code":100,
--       "error_data":"No matching user found.",
--       "fbtrace_id":"D2kxCybrKVw"
--    }
--  
data SendErrorObject = SendErrorObject { eoMessage :: Text, eoType :: Text, eoCode :: SendErrorCode, eoErrorData :: Text, eoFbtraceId :: Text } deriving (Eq, Show, Generic)
                           
instance ToJSON SendErrorObject where
  toJSON SendErrorObject{..} = object [ "error" .= e ]
    where e = object [ "message"    .= eoMessage
                     , "type"       .= eoType
                     , "code"       .= eoCode
                     , "error_data" .= eoErrorData
                     , "fbtrace_id" .= eoFbtraceId ]
  
instance FromJSON SendErrorObject where
  parseJSON = withObject "send error" $ \o ->
    let e = o .: "error"
        e' field = e >>= (.: field)
    in SendErrorObject <$>
       e' "message"    <*>
       e' "type"       <*>
       e' "code"       <*>
       e' "error_data" <*>
       e' "fbtrace_id"  


-- | Send API Error Codes (see: https://developers.facebook.com/docs/messenger-platform/send-api-reference#errors)
--
--   Code	Description
--   2     Send message failure. Internal server error.
--   10    Application does not have permission to use the Send API
--   100   No matching user found
--   613   Calls to this api have exceeded the rate limit.
--
data SendErrorCode = InternalServerError | UnauthorizedApplication | NoMatchingUserFound | RateLimitError deriving (Eq, Show) 
instance ToJSON SendErrorCode where
  toJSON InternalServerError     = Number 2
  toJSON UnauthorizedApplication = Number 10
  toJSON NoMatchingUserFound     = Number 100
  toJSON RateLimitError          = Number 613
instance FromJSON SendErrorCode where
  parseJSON (Number 2)   = pure InternalServerError
  parseJSON (Number 10)  = pure UnauthorizedApplication
  parseJSON (Number 100) = pure NoMatchingUserFound
  parseJSON (Number 613) = pure RateLimitError
  parseJSON _            = fail "Unable to parse SendErrorCode" 


-- | Take a Send API error object and return a tuple containing the error code 
--   and the error_data (seems to always be the description)
errorInfo :: SendErrorObject -> (SendErrorCode, Text)
errorInfo err = (ecode, edata) 
  where edata = eoErrorData err
        ecode = eoCode err

-- | Extracts a Send API Error object from the 'ServantError' (when possible).
extractSendError :: ServantError -> Maybe SendErrorObject
extractSendError FailureResponse{ responseStatus = _, responseContentType = _
                                , responseBody = body 
                                } = decode body :: Maybe SendErrorObject
extractSendError _ = Nothing
