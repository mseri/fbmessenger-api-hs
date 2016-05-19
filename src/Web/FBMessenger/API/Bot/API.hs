{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.FBMessenger.API.Bot.API 
  ( -- * API
    FBMessengerBotAPI
  --, api
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
import           Servant.API
import           Servant.Client
import           Web.FBMessenger.API.Bot.Requests
import           Web.FBMessenger.API.Bot.Responses

-- | Messenger Platform PAGE_ACCESS_TOKEN
newtype Token = Token Text
   deriving (Show, Eq, Ord, ToHttpApiData, FromHttpApiData)

-- | Type for token
type GraphAPIAccessToken = QueryParam "access_token" Token

-- from Servant.Client
graphAPIBaseUrl :: BaseUrl
graphAPIBaseUrl = BaseUrl Https "graph.facebook.com" 443 "v2.6/me"

-- | Messenger Platform Bot API
type FBMessengerBotAPI = 
         GraphAPIAccessToken :> "messages" 
         :> Post '[JSON] SendTextMessageRequest
    :<|> GraphAPIAccessToken :> "subscribed_apps"
         :> Post '[JSON] SubscriptionResponse
    
     