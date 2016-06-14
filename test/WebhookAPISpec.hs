{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module WebhookAPISpec (spec) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath
import           System.IO.Unsafe
import           Test.Hspec
import           Web.FBMessenger.API.Bot.WebhookAPI

import           Paths_fbmessenger_api

run acBSLn name = unsafePerformIO $ do
    dataDir <- getDataDir
    let testFile name = dataDir </> "test-files" </> name
    content <- BSL.readFile $ testFile name
    return $ acBSLn content

-- TODO: add more tests for other cases, failures or corner cases

spec :: Spec
spec = do
    let pid = "111111"
    let sid = "USER_ID"
    let rid = "PAGE_ID"
    
    let authMessage = EventMessage sid rid (Just 1234567890) $ EmAuth "PASS_THROUGH_PARAM"
    let authEvent = RemoteEvent pid 12341 [authMessage]
    let ar = RemoteEventList [authEvent]
    
    let deliveryMessage = EventMessage sid rid Nothing $ EmDelivery 37 1458668856253 (Just ["mid.1458668856218:ed81099e15d3f4f233"])
    let deliveryEvent = RemoteEvent pid 1458668856451 [deliveryMessage]
    let dr = RemoteEventList [deliveryEvent]
    
    let postbackMessage = EventMessage sid rid (Just 1458692752478) $ EmPostback "USER_DEFINED_PAYLOAD"
    let postbackEvent = RemoteEvent pid 1458692752478 [postbackMessage]
    let pr = RemoteEventList [postbackEvent]
    
    let textMessage = EventMessage sid rid (Just 1457764197627) $ EmTextMessage "mid.1457764197618:41d102a3e1ae206a38" 73 "hello, world!"
    let textEvent = RemoteEvent pid 1457764198246 [textMessage]
    let tr = RemoteEventList [textEvent]
    
    let structuredMessage = EventMessage sid rid (Just 1458696618268) $ EmStructuredMessage "mid.1458696618141:b4ef9d19ec21086067" 51 [EmAttachment EmImage "IMAGE_URL"]
    let structuredMessageEvent = RemoteEvent pid 1458696618911 [structuredMessage]
    let sr = RemoteEventList [structuredMessageEvent]
    
    describe "webhook request parsing and generation" $ do              
         it "auth message is parsed properly" $
             decodeWR "wsAuthRequest.json" ar
         
         it "delivery message is parsed properly" $
             decodeWR "wsDeliveryRequest.json" dr

         it "postback message is parsed properly" $
             decodeWR "wsPostbackRequest.json" pr

         it "text message is parsed properly" $
             decodeWR "wsTextMessageRequest.json" tr

         it "structured message is parsed properly" $
             decodeWR "wsStructuredMessageRequest.json" sr
    
    describe "webhook request serialization" $ do
         it "auth message is serialized properly" $ 
             checkWR ar
         
         it "delivery message is serialized properly" $
             checkWR dr

         it "postback message is serialized properly" $
             checkWR pr
             
         it "text message is serialized properly" $
             checkWR tr

         it "structured message is serialized properly" $
             checkWR sr
     
     where
         decodeWR fName m = run (\l -> eitherDecode l :: Either String RemoteEventList) fName `shouldBe` Right m
         checkWR m = (eitherDecode $ encode m :: Either String RemoteEventList) `shouldBe` Right m
