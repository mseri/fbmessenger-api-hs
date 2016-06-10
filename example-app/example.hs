{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when, void)
import Control.Monad.Trans.Except (ExceptT)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text as T hiding (map)
import Network.HTTP.Client hiding (Proxy, port)
import Network.HTTP.Client.TLS
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO
import Web.FBMessenger.API.Bot

type WebHookAPI = "webhook" :> 
    QueryParam "hub.verify_token" String :> 
    QueryParam "hub.challenge" String :> 
    Get '[PlainText] String
  :<|> "webhook" :>
    ReqBody '[JSON] RemoteEventList :>
    Post '[PlainText] String

webHookAPI :: Proxy WebHookAPI
webHookAPI = Proxy

server :: String -> Server WebHookAPI
server verifyTokenStored = 
  webhook_verify
  :<|> webhook_message
  where
    webhook_verify :: Maybe String -> Maybe String -> ExceptT ServantErr IO String
    webhook_verify (Just verifyToken) (Just challenge) 
      | verifyToken == verifyTokenStored = return challenge
    webhook_verify _ _ = throwError err500 { errBody = "Error, wrong validation token"}

    webhook_message :: RemoteEventList -> ExceptT ServantErr IO String
    webhook_message (RemoteEventList res) = do
      let _ = map (echoMessage . evt_messaging) res
      return "ok"

    echoMessage :: [EventMessage] -> IO ()
    echoMessage msgs = void (mapM_ process msgs)
      where
        token = Token $ T.pack verifyTokenStored

        process msg = 
          case evtContent msg of
            EmTextMessage _ _ text -> echoTextMessage text (evtSenderId msg)
            _                      -> void (putStrLn "LOG: this is just an example, no complex message is echoed.")

        echoTextMessage text rcptId = do 
          m <- newManager tlsManagerSettings
          -- create the recipient using the sender id
          -- (we know that this is successful when only one argument is not Nothing) 
          let (Just rcpt) = recipient (Just rcptId) Nothing
          -- prepare the Send API request body 
          let messageReq = sendTextMessageRequest Nothing rcpt text
          putStrLn ("LOG: sending " ++ T.unpack text ++ " to " ++ show rcpt)
          -- finally send the message request using the tls http connection manager  
          void $ sendTextMessage (Just token) messageReq m
    

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 3000 read $ lookup "PORT" env
    let verifyToken = fromMaybe "" $ lookup "VERIFY_TOKEN" env
    when (verifyToken == "") (putStrLn "Please set VERIFY_TOKEN to a safe string")
    putStrLn $ "Server listening on port " ++ show port
    run port $ serve webHookAPI $ server verifyToken 
