{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

type WebHookAPI = "webhook" :> 
  QueryParam "hub.verify_token" String :> 
  QueryParam "hub.challenge" String :> 
  Get '[PlainText] String

webHookAPI :: Proxy WebHookAPI
webHookAPI = Proxy

server :: String -> Server WebHookAPI
server verifyTokenStored = webhook
  where
    webhook :: Maybe String -> Maybe String -> ExceptT ServantErr IO String
    webhook (Just verifyToken) (Just challenge) 
      | verifyToken == verifyTokenStored = return challenge
    webhook _ _ = throwError err500 { errBody = "Error, wrong validation token"}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 3000 read $ lookup "PORT" env
    let verifyToken = fromMaybe "" $ lookup "VERIFY_TOKEN" env
    when (verifyToken == "") (putStrLn "Please set VERIFY_TOKEN to a safe string")
    putStrLn $ "Server listening on port " ++ show port
    run port $ serve webHookAPI $ server verifyToken 
