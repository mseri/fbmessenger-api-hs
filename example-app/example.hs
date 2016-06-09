{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
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
    echoMessage msgs = mapM_ process msgs >> return ()
      where 
        process msg = case evtContent msg of
            EmTextMessage _ _ text -> do
              case recipient (Just $ evtSenderId msg) Nothing of
                Nothing -> return ()
                Just r -> do
                  let req = sendTextMessageRequest Nothing r text
                  m <- newManager tlsManagerSettings
                  let t = Token $ T.pack verifyTokenStored
                  let _ = sendTextMessage (Just $ t) req m
                  return ()
            _ -> return ()


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 3000 read $ lookup "PORT" env
    let verifyToken = fromMaybe "" $ lookup "VERIFY_TOKEN" env
    when (verifyToken == "") (putStrLn "Please set VERIFY_TOKEN to a safe string")
    putStrLn $ "Server listening on port " ++ show port
    run port $ serve webHookAPI $ server verifyToken 
