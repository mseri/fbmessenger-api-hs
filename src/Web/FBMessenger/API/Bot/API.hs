{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.FBMessenger.API.Bot.API 
  ( -- * Functions
    sendTextMessage
  , subscribedApps
    -- * API
  , FBMessengerBotAPI
  , api
    -- * Types
  , Token             (..)
  ) where

-- ExceptT is practically the same as EitherT
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Text (Text)
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

-- from Servant.Client
graphAPIBaseUrl :: BaseUrl
graphAPIBaseUrl = BaseUrl Https "graph.facebook.com" 443 "v2.6/me"

-- | Messenger Platform Bot API
type FBMessengerBotAPI = 
         GraphAPIAccessToken :> "messages" 
         :> ReqBody '[JSON] SendTextMessageRequest
         :> Post '[JSON] MessageResponse
    :<|> GraphAPIAccessToken :> "subscribed_apps"
         :> Post '[JSON] SubscriptionResponse
    
-- | Proxy for Messenger Platform Bot API
api :: Proxy FBMessengerBotAPI
api = Proxy

sendTextMessage_ :: Maybe Token -> SendTextMessageRequest -> Manager -> BaseUrl -> ExceptT ServantError IO MessageResponse
subscribedApps_  :: Maybe Token -> Manager -> BaseUrl -> ExceptT ServantError IO SubscriptionResponse

sendTextMessage_
  :<|> subscribedApps_ = client api

-- | Use this method to send text messages. On success, the sent 'Message' is returned.
sendTextMessage :: Maybe Token -> SendTextMessageRequest -> Manager -> IO (Either ServantError MessageResponse)
sendTextMessage = run graphAPIBaseUrl sendTextMessage_

-- | A simple method for testing your bot's auth token. Requires no parameters.
--   Returns a simple object containing a boolean value indicating if the token is correctly registered.
subscribedApps :: Maybe Token -> Manager -> IO (Either ServantError SubscriptionResponse)
subscribedApps token manager = runExceptT $ subscribedApps_ token manager graphAPIBaseUrl

run :: BaseUrl -> (Maybe Token -> a -> Manager -> BaseUrl -> ExceptT ServantError IO b) -> Maybe Token -> a -> Manager -> IO (Either ServantError b)
run b e t r m = runExceptT $ e t r m b