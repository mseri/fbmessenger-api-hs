{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as C
--import Data.Either (either)
import           Data.Foldable              (traverse_)
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy
import qualified Data.Text                  as T
import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger         (withStdoutLogger)
import           Servant
import           System.Environment
import           System.Exit                (exitFailure)
import           System.IO
import           Text.Printf                (printf)
import           Web.FBMessenger.API.Bot

type WebHookAPI = "webhook" :>
    QueryParam "hub.verify_token" String :>
    QueryParam "hub.challenge" String :>
    Get '[PlainText] String
  :<|> "webhook" :>
    ReqBody '[JSON] RemoteEventList :>
    Post '[PlainText,JSON] String

webHookAPI :: Proxy WebHookAPI
webHookAPI = Proxy

-- TODO: use monad-logger to implement proper logging

server :: String -> String -> Server WebHookAPI
server verifyTokenStored pageTokenStored =
  webhook_verify
  :<|> webhook_message
  where
    webhook_verify (Just verifyToken) (Just challenge)
      | verifyToken == verifyTokenStored = return challenge
    webhook_verify tk ch = let
      eB :: String
      eB = printf "[ERROR]: wrong validation request. Got (token, challenge) = (%s, %s)" (show tk) (show ch)
      in do
        liftIO $ putStrLn eB
        throwError err500 { errBody = C.pack eB}

    webhook_message (RemoteEventList res) = do
      r <- traverse echoMessage (concatMap evt_messaging res)
      liftIO $ traverse_ (putStrLn . T.unpack) r
      return "{\"status\":\"fulfilled\"}"

    token = Token $ T.pack pageTokenStored

    echoMessage msg =
      case evtContent msg of
        EmTextMessage _ _ text -> liftIO $ echoTextMessage text (evtSenderId msg)
        _                      -> return "[WARN]: this is just an example, no complex message is echoed."

    echoTextMessage text rcptId = do
      m <- newManager tlsManagerSettings
      -- create the recipient using the sender id
      -- (we know that this is successful when only one argument is not Nothing)
      let (Just rcpt) = recipient (Just rcptId) Nothing
      -- prepare the Send API request body
      let messageReq = sendTextMessageRequest Nothing rcpt text
      -- finally send the message request using the tls http connection manager
      logRsp <- sendTextMessage (Just token) messageReq m
      -- restructure the response to get a decent log out of it
      -- e.g. one could add the response or the error in the log message
      let logMsg = either (\_ -> printf "[ERROR]: failed to echo message")
                          (\_ -> printf "[INFO]: sent \"%s\" to %s" (T.unpack text) (show rcpt))
                          logRsp
      return (T.pack logMsg)

app :: String -> String -> Application
app verifyToken pageToken = serve webHookAPI $ server verifyToken pageToken

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment

    let port = maybe 3000 read $ lookup "PORT" env

    let verifyToken = fromMaybe "" $ lookup "VERIFY_TOKEN" env
    when (verifyToken == "") $ do
      putStrLn "[WARN]: please set VERIFY_TOKEN to a safe string"
      exitFailure

    let pageToken = fromMaybe "" $ lookup "PAGE_TOKEN" env
    when (pageToken == "") $ putStrLn "[WARN]: make sure to set up the correct page token"

    putStrLn $ printf "[INFO]: server listening on port %i" port
    withStdoutLogger $ \aplogger -> do
      let settings = setPort port $ setLogger aplogger defaultSettings
      runSettings settings (app verifyToken pageToken)
