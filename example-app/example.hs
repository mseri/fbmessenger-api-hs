{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO
import Text.Printf (printf)
import Web.FBMessenger.API.Bot

type WebHookAPI = "webhook" :> 
    QueryParam "hub.verify_token" String :> 
    QueryParam "hub.challenge" String :> 
    Get '[PlainText] String
  :<|> "webhook" :>
    ReqBody '[JSON] RemoteEventList :>
    Post '[PlainText,JSON] String

webHookAPI :: Proxy WebHookAPI
webHookAPI = Proxy

-- TODO: use monad-logger for logging

server :: String -> Server WebHookAPI
server verifyTokenStored = 
  webhook_verify
  :<|> webhook_message
  where
    webhook_verify :: Maybe String -> Maybe String -> ExceptT ServantErr IO String
    webhook_verify (Just verifyToken) (Just challenge) 
      | verifyToken == verifyTokenStored = return challenge
    webhook_verify tk ch = let
      eB :: String
      eB = printf "[ERROR]: wrong validation request. Got (token, challenge) = (%s, %s)" (show tk) (show ch)
      in throwError err500 { errBody = C.pack eB}

    webhook_message :: RemoteEventList -> ExceptT ServantErr IO String
    webhook_message (RemoteEventList res) = do
      r <- traverse echoMessage (concatMap evt_messaging res)
      liftIO $ traverse_ (putStrLn . show) r
      return "{\"status\":\"fulfilled\"}"

    token = Token $ T.pack verifyTokenStored

    echoMessage :: EventMessage -> ExceptT ServantErr IO T.Text
    echoMessage msg = do
      case evtContent msg of
        EmTextMessage _ _ text -> liftIO $ echoTextMessage text (evtSenderId msg) 
        _                      -> return "[LOG]: this is just an example, no complex message is echoed."
    
    echoTextMessage text rcptId = do 
      m <- newManager tlsManagerSettings
      -- create the recipient using the sender id
      -- (we know that this is successful when only one argument is not Nothing) 
      let (Just rcpt) = recipient (Just rcptId) Nothing
      -- prepare the Send API request body 
      let messageReq = sendTextMessageRequest Nothing rcpt text
      -- finally send the message request using the tls http connection manager  
      logMsg <- sendTextMessage (Just token) messageReq m
      return (T.pack $ "[LOG]: sending " ++ T.unpack text ++ " to " ++ show rcpt 
                       ++ ".\n [LOG]: response" ++ show logMsg)
    

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 3000 read $ lookup "PORT" env
    let verifyToken = fromMaybe "" $ lookup "VERIFY_TOKEN" env
    when (verifyToken == "") (putStrLn "[WARN]: Please set VERIFY_TOKEN to a safe string")
    putStrLn $ "[LOG]: Server listening on port " ++ show port
    run port $ serve webHookAPI $ server verifyToken 
