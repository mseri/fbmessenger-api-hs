{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ResponsesSpec (spec) where

import           Control.Monad
import           Data.Aeson
import           Data.Either (isRight, isLeft)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as TIO
import           System.FilePath
import System.IO.Unsafe
import           Test.Hspec
import           Web.FBMessenger.API.Bot.Responses

import           Paths_fbmessenger_api

-- -- to print out remote response if response success not match
-- success, nosuccess :: (Show a, Show b) =>Either a b ->Expectation
-- success   e = e `shouldSatisfy` isRight
-- nosuccess e = e `shouldSatisfy` isLeft

-- spec :: Token -> Text -> Text -> Spec
-- spec token chatId botName = do
--   manager <- runIO $ newManager tlsManagerSettings
--   describe "/getMe" $ do
--     it "responds with correct bot's name" $ do
--       Right GetMeResponse { user_result = u } <-
--         getMe token manager
--       (user_first_name u) `shouldBe` botName -- f.e. "TelegramAPIBot"

--   describe "/sendMessage" $ do
--     it "should send message" $ do
--       res <- sendMessage token (sendMessageRequest chatId "test message") manager
--       success res
--       let Right MessageResponse { message_result = m } = res
--       (text m) `shouldBe` (Just "test message")

run action name = unsafePerformIO $ do
    dataDir <- getDataDir
    let testFile name = dataDir </> "test-files" </> name
    content <- TIO.readFile $ testFile name
    return $ action content

spec :: Spec
spec = do
    describe "response parsing" $ do
        it "subscription response is parsed properly" $
            run (\l -> decode l :: Maybe SubscriptionResponse) "subscriptionResponse.json"
            `shouldBe`
            Just (SubscriptionResponse True)
        
        it "welcome message response is parsed properly" $
            run (\l -> decode l :: Maybe WelcomeMessageResponse) "welcomeMessageResponse.json"
            `shouldBe`
            Just (WelcomeMessageResponse "Successfully added new_thread's CTAs")
    describe "error response parsing" $ do
        let ewo = run (\l -> decode l :: Maybe SendErrorWrapperObject) "errorResponse.json"
        let eo  = SendErrorObject "Invalid parameter" "FacebookApiException" NoMatchingUserFound "No matching user found." "D2kxCybrKVw"
        it "error response is parsed properly" $
            ewo `shouldBe` Just (SendErrorWrapperObject eo)
        
        it "error response is extracted properly" $ 
            errorInfo eo `shouldBe` (NoMatchingUserFound, "No matching user found.")
    
        -- TODO: Error responses
--         {
--    "error":{
--       "message":"Invalid parameter",
--       "type":"FacebookApiException",
--       "code":100,
--       "error_data":"No matching user found.",
--       "fbtrace_id":"D2kxCybrKVw"
--    }
-- }