{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.FBMessenger.API.Bot.SendAPI 
  ( -- * Functions
    getUserProfileInfo
  , removeWelcomeMessage
  , sendTextMessage
  , sendStructuredMessage
  , setWelcomeMessage
  , subscribedApps
    -- * API
  , FBMessengerSendAPI
  , api
    -- * Types
  , Token                  (..)
  ) where

-- ExceptT is practically the same as EitherT
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client
import           Web.FBMessenger.API.Bot.Requests
import           Web.FBMessenger.API.Bot.Responses


-- | Messenger Platform PAGE_ACCESS_TOKEN
newtype Token = Token Text
   deriving (Show, Eq, Ord, ToHttpApiData, FromHttpApiData)

-- | Type for token
-- NOTE: QueryParam here gives us a Maybe Token
type GraphAPIAccessToken = QueryParam "access_token" Token


-- Servant.Client.BaseUrl
graphAPIBaseUrl :: BaseUrl
graphAPIBaseUrl = BaseUrl Https "graph.facebook.com" 443 "/v2.6/me"


-- | Messenger Platform Send API
type FBMessengerSendAPI = 
         GraphAPIAccessToken :> "messages" 
         :> ReqBody '[JSON] SendTextMessageRequest
         :> Post '[JSON] MessageResponse
    :<|> GraphAPIAccessToken :> "messages" 
         :> ReqBody '[JSON] SendStructuredMessageRequest
         :> Post '[JSON] MessageResponse
    :<|> GraphAPIAccessToken :> "subscribed_apps"
         :> Post '[JSON] SubscriptionResponse
    :<|> GraphAPIAccessToken :> Capture "page_id" Text :> "thread_settings"
         :> ReqBody '[JSON] WelcomeMessageRequest
         :> Post '[JSON] WelcomeMessageResponse
    :<|> GraphAPIAccessToken :> Capture "page_id" Text :> "thread_settings"
         :> ReqBody '[JSON] WelcomeMessageRequest
         :> Delete '[JSON] WelcomeMessageResponse
    :<|> GraphAPIAccessToken :> QueryParam "fields" Text :> Capture "user_id" Text
         :> Get '[JSON] UserProfileResponse


-- | Proxy for Messenger Platform Bot Send
api :: Proxy FBMessengerSendAPI
api = Proxy

sendTextMessage_       ::        Maybe Token -> SendTextMessageRequest -> Manager -> BaseUrl -> ExceptT ServantError IO MessageResponse
sendStructuredMessage_ ::  Maybe Token -> SendStructuredMessageRequest -> Manager -> BaseUrl -> ExceptT ServantError IO MessageResponse
subscribedApps_        ::                                  Maybe Token -> Manager -> BaseUrl -> ExceptT ServantError IO SubscriptionResponse
welcomeMessage_        :: Maybe Token -> Text -> WelcomeMessageRequest -> Manager -> BaseUrl -> ExceptT ServantError IO WelcomeMessageResponse
deleteWMessage_        :: Maybe Token -> Text -> WelcomeMessageRequest -> Manager -> BaseUrl -> ExceptT ServantError IO WelcomeMessageResponse
userProfile_           ::            Maybe Token -> Maybe Text -> Text -> Manager -> BaseUrl -> ExceptT ServantError IO UserProfileResponse

sendTextMessage_
  :<|> sendStructuredMessage_
  :<|> subscribedApps_ 
  :<|> welcomeMessage_
  :<|> deleteWMessage_
  :<|> userProfile_ = client api


-- | Send text messages. On success, minor informations on the sent message are returned.
sendTextMessage :: Maybe Token -> SendTextMessageRequest -> Manager -> IO (Either ServantError MessageResponse)
sendTextMessage = run graphAPIBaseUrl sendTextMessage_

-- | Send structured messages containing an image. On success, minor informations on the sent message are returned.
sendStructuredMessage :: Maybe Token -> SendStructuredMessageRequest -> Manager -> IO (Either ServantError MessageResponse)
sendStructuredMessage = run graphAPIBaseUrl sendStructuredMessage_

-- | Test if your bot's auth token is enabled. Requires no parameters.
--   Returns a simple object containing a boolean value indicating if the token is correctly registered.
subscribedApps :: Maybe Token -> Manager -> IO (Either ServantError SubscriptionResponse)
subscribedApps token manager = runExceptT $ subscribedApps_ token manager graphAPIBaseUrl

-- | Set a welcome message. In addition to the token and the message request, you need to provide the facebook page_id.
--   Returns a simple object containing a string indicating if the welcome message is correctly registered.
setWelcomeMessage :: Maybe Token -> Text -> WelcomeMessageRequest -> Manager -> IO (Either ServantError WelcomeMessageResponse)
setWelcomeMessage token pageId message manager = runExceptT $ welcomeMessage_ token pageId message manager graphAPIBaseUrl

-- | Remove the welcome message. In addition to the token, you need to provide the facebook page_id.
--   Returns a simple object containing a string indicating if the welcome message is correctly removed.
removeWelcomeMessage :: Maybe Token -> Text -> Manager -> IO (Either ServantError WelcomeMessageResponse)
removeWelcomeMessage token pageId manager = runExceptT $ deleteWMessage_ token pageId welcomeDeleteMessage manager graphAPIBaseUrl

-- | Get the profile informations of a user. In addition to the token, you need to provide the user_id.
--   Returns a record containing the profile informations.
getUserProfileInfo :: Token -> Text -> Manager -> ExceptT ServantError IO UserProfileResponse
getUserProfileInfo token userdId manager = runExceptT $ userProfile_ token userProfileFields userId manager graphAPIBaseUrl


-- Helpers (not exported)

userProfileFields :: Maybe Text
userProfileFields = pure $ T.pack "first_name,last_name,profile_pic,locale,timezone,gender"

welcomeDeleteMessage :: WelcomeMessageRequest
welcomeDeleteMessage = WelcomeMessageRequest 
  { wm_setting_type    = T.pack "call_to_actions"
  , wm_thread_state    = T.pack "new_thread"
  , wm_call_to_actions = [] }

run :: BaseUrl -> (Maybe Token -> a -> Manager -> BaseUrl -> ExceptT ServantError IO b) -> Maybe Token -> a -> Manager -> IO (Either ServantError b)
run b e t r m = runExceptT $ e t r m b
