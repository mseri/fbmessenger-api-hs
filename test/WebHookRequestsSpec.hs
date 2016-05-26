{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module WebHookRequestsSpec (spec) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath
import           System.IO.Unsafe
import           Test.Hspec
import           Web.FBMessenger.API.Bot.WebHookRequests

import           Paths_fbmessenger_api

run acBSLn name = unsafePerformIO $ do
    dataDir <- getDataDir
    let testFile name = dataDir </> "test-files" </> name
    content <- BSL.readFile $ testFile name
    return $ acBSLn content

spec :: Spec
spec = do
    let pageId = 111111
    let entryTime = 12341
    let authMessage = WRMessage "USER_ID" "PAGE_ID" 1234567890 (WRMAuth "PASS_THROUGH_PARAM")
    let authEvent = WREvent pageId entryTime [authMessage]
    let ar = WRRequest [authEvent]
    
    describe "webhook request parsing and generation" $ do              
         it "auth message is parsed properly" $
             run (\l -> eitherDecode l :: Either String WRRequest) "wsAuthRequest.json" `shouldBe` Right ar
        
--         it "generic template message is parsed properly" $
--             decodeSSm "genericTemplate.json" gm
        
--         it "button template message is parsed properly" $
--             decodeSSm "buttonTemplate.json" bm
        
--         it "receipt template message is parsed properly" $
--             decodeSSm "receiptTemplate.json" rm
    
    describe "webhook request serialization" $ do
         it "auth message is serialized properly" $ 
             (eitherDecode $ encode ar :: Either String WRRequest) `shouldBe` Right ar
        
--         it "generic template message is serialized properly" $
--             checkSSm gm
        
--         it "button template message is serialized properly" $
--             checkSSm bm
        
--         it "receipt template message is serialized properly" $
--             checkSSm rm
     
--      where
--          decodeSSm fName m = run (\l -> eitherDecode l :: Either String SendStructuredMessageRequest) fName `shouldBe` Right m
--          checkSSm m = (eitherDecode $ encode m :: Either String SendStructuredMessageRequest) `shouldBe` Right m

-- -- TODO: Consider adding additional tests for corner cases and failures
