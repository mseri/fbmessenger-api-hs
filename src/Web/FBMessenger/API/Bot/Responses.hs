{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- | This module contains responses from Messenger Platform Bot API
module Web.FBMessenger.API.Bot.Responses 
  ( -- * Types 
    SendErrorCode          (..)
  , SendErrorObject        (..)
  , MessageResponse        (..)
  , SubscriptionResponse   (..)
  , UserProfileResponse    (..)
  , WelcomeMessageResponse (..)
  -- * Functions
  , sendErrorInfo
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


data SendErrorWrapperObject = SendErrorWrapperObject { ewo_error :: SendErrorObject } deriving (Eq, Show, Generic)
instance ToJSON SendErrorWrapperObject where
  toJSON = toJsonDrop 4
instance FromJSON SendErrorWrapperObject where
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
data SendErrorObject = SendErrorObject { eo_message :: Text, eo_type :: Text, eo_code :: Int, eo_error_data :: Text, eo_fbtrace_id :: Text } deriving (Eq, Show, Generic)
instance ToJSON SendErrorObject where
  toJSON = toJsonDrop 3
instance FromJSON SendErrorObject where
  parseJSON = parseJsonDrop 3

-- | Send API Error Codes (see: https://developers.facebook.com/docs/messenger-platform/send-api-reference#errors)
--
--   Code	Description
--   2     Send message failure. Internal server error.
--   10    Application does not have permission to use the Send API
--   100   No matching user found
--   613   Calls to this api have exceeded the rate limit.
--
data SendErrorCode = InternalServerError | UnauthorizedApplication | NoMatchingUserFound | RateLimitError | Other deriving (Eq, Show) 

-- | Take a Send API error object and return a tuple containing the error code and the error_data (seems to always be the description)
sendErrorInfo :: SendErrorObject -> (SendErrorCode, Text)
sendErrorInfo err = (ecode, edata) 
  where edata = eo_error_data err
        ecode = case eo_code err of
                  2   -> InternalServerError
                  10  -> UnauthorizedApplication
                  100 -> NoMatchingUserFound
                  613 -> RateLimitError
                  _   -> Other

-- | Extracts a Send API Error object from the ServantError (when possible).
extractSendError :: ServantError -> Maybe SendErrorObject
extractSendError FailureResponse{ responseStatus = _, responseContentType = _
                                , responseBody = body 
                                } = do
                                  ewo <- decode body :: Maybe SendErrorWrapperObject
                                  return $ ewo_error ewo
extractSendError _ = Nothing
