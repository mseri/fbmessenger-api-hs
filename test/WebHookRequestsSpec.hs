{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- module RequestsSpec (spec) where

-- import           Data.Aeson
-- import qualified Data.ByteString.Lazy as BSL
-- import           System.FilePath
-- import           System.IO.Unsafe
-- import           Test.Hspec
-- import           Web.FBMessenger.API.Bot.WebHookRequests

-- import           Paths_fbmessenger_api

-- run acBSLn name = unsafePerformIO $ do
--     dataDir <- getDataDir
--     let testFile name = dataDir </> "test-files" </> name
--     content <- BSL.readFile $ testFile name
--     return $ acBSLn content

-- spec :: Spec
-- spec = do
    

--     describe "request parsing and generation" $ do              
--         it "image message is parsed properly" $
--             decodeSSm "imageMessage.json" im
        
--         it "generic template message is parsed properly" $
--             decodeSSm "genericTemplate.json" gm
        
--         it "button template message is parsed properly" $
--             decodeSSm "buttonTemplate.json" bm
        
--         it "receipt template message is parsed properly" $
--             decodeSSm "receiptTemplate.json" rm
    
--     describe "request serialization" $ do
--         it "image message is serialized properly" $ 
--             checkSSm im
        
--         it "generic template message is serialized properly" $
--             checkSSm gm
        
--         it "button template message is serialized properly" $
--             checkSSm bm
        
--         it "receipt template message is serialized properly" $
--             checkSSm rm
     
--      where
--          decodeSSm fName m = run (\l -> eitherDecode l :: Either String SendStructuredMessageRequest) fName `shouldBe` Right m
--          checkSSm m = (eitherDecode $ encode m :: Either String SendStructuredMessageRequest) `shouldBe` Right m

-- TODO: Consider adding additional tests for corner cases and failures
