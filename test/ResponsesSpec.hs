{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ResponsesSpec (spec) where

import           Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath
import           System.IO.Unsafe
import           Test.Hspec
import           Web.FBMessenger.API.Bot.Responses

import           Paths_fbmessenger_api

run acBSLn name = unsafePerformIO $ do
    dataDir <- getDataDir
    let testFile name = dataDir </> "test-files" </> name
    content <- BSL.readFile $ testFile name
    return $ acBSLn content

spec :: Spec
spec = do
    describe "response parsing" $ do
        it "subscriptionn response is parsed properly" $
            run (\l -> decode l :: Maybe SubscriptionResponse) "subscriptionResponse.json"
            `shouldBe`
            Just (SubscriptionResponse True)
        
        it "welcome message response is parsed properly" $
            run (\l -> decode l :: Maybe WelcomeMessageResponse) "welcomeMessageResponse.json"
            `shouldBe`
            Just (WelcomeMessageResponse "Successfully added new_thread's CTAs")
            
    describe "error response parsing" $ do
        let ewo = run (\l -> decode l :: Maybe SendErrorObject) "errorResponse.json"
        let eo  = SendErrorObject "Invalid parameter" "FacebookApiException" NoMatchingUserFound "No matching user found." "D2kxCybrKVw"
        
        it "error response is parsed properly" $
            ewo `shouldBe` Just eo
        
        it "error response is extracted properly" $ 
            errorInfo eo `shouldBe` (NoMatchingUserFound, "No matching user found.")
    
        -- extractSendError not tested