{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module RequestsSpec (spec) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath
import           System.IO.Unsafe
import           Test.Hspec
import           Web.FBMessenger.API.Bot.Requests

import           Paths_fbmessenger_api

run acBSLn name = unsafePerformIO $ do
    dataDir <- getDataDir
    let testFile name = dataDir </> "test-files" </> name
    content <- BSL.readFile $ testFile name
    return $ acBSLn content

spec :: Spec
spec = do
    let rcpt = Recipient (Just "USER_ID") Nothing
    let tm = sendTextMessageRequest Nothing rcpt "hello, world!"
    let im = sendImageMessageRequest Nothing rcpt "https://petersapparel.com/img/shirt.png"
    
    let btn1 = webUrlButton "View Item" "https://petersapparel.parseapp.com/view_item?item_id=100"
    let btn1' = webUrlButton "Buy Item" "https://petersapparel.parseapp.com/buy_item?item_id=100"
    let btn1'' = postbackButton "Bookmark Item" "USER_DEFINED_PAYLOAD_FOR_ITEM100"
    let btns1 = [btn1, btn1', btn1'']
    
    let btn2 = webUrlButton "View Item" "https://petersapparel.parseapp.com/view_item?item_id=101"
    let btn2' = webUrlButton "Buy Item" "https://petersapparel.parseapp.com/buy_item?item_id=101"
    let btn2'' = postbackButton "Bookmark Item" "USER_DEFINED_PAYLOAD_FOR_ITEM101"
    let btns2 = [btn2, btn2', btn2'']
    
    let b1 = BubbleElement "Classic White T-Shirt" Nothing (Just "http://petersapparel.parseapp.com/img/item100-thumb.png") (Just "Soft white cotton t-shirt is back in style") (Just btns1)
    let b2 = BubbleElement "Classic Grey T-Shirt" Nothing (Just "http://petersapparel.parseapp.com/img/item101-thumb.png") (Just "Soft gray cotton t-shirt is back in style") (Just btns2)
    
    let gm = sendGenericTemplateMessageRequest Nothing rcpt [b1, b2]
    
    let bm1 = webUrlButton "Show Website" "https://petersapparel.parseapp.com"
    let bm2 = postbackButton "Start Chatting" "USER_DEFINED_PAYLOAD"
    let bm = sendButtonTemplateMessageRequest Nothing rcpt "What do you want to do next?" [bm1, bm2]  
    
    let e1 = ReceiptItem "Classic White T-Shirt" (Just "100% Soft and Luxurious Cotton") (Just 2) (Just 50) (Just "USD") (Just "http://petersapparel.parseapp.com/img/whiteshirt.png")
    let e2 = ReceiptItem "Classic Gray T-Shirt" (Just "100% Soft and Luxurious Cotton") (Just 1) (Just 25) (Just "USD") (Just "http://petersapparel.parseapp.com/img/grayshirt.png")
    let ems = [e1, e2]
    
    let sa = ShippingAddress "1 Hacker Way" (Just "") "Menlo Park" "94025" "CA" "US"
    let ps = PaymentSummary (Just 75.00) (Just 4.95) (Just 6.19) 56.14
    
    let aa1 = PaymentAdjustment (Just "New Customer Discount") (Just 20)
    let aa2 = PaymentAdjustment (Just "$10 Off Coupon") (Just 10)
    let aas = [aa1, aa2]
    
    let rm = sendReceiptTemplateMessageRequest Nothing rcpt "Stephane Crozatier" "12345678902" "USD" "Visa 2345" (Just "1428444852") (Just "http://petersapparel.parseapp.com/order?order_id=123456") ems (Just sa) ps (Just aas)

    describe "request parsing and generation" $ do      
        it "recipient is generated succesfully" $ 
            Just rcpt `shouldBe` recipient (Just "USER_ID") Nothing
            
        it "text message is parsed properly" $
            run (\l -> decode l :: Either String SendTextMessageRequest) "textMessage.json"
            `shouldBe` Right tm
        
        it "image message is parsed properly" $
            decodeSSm "imageMessage.json" im
        
        it "generic template message is parsed properly" $
            decodeSSm "genericTemplate.json" gm
        
        it "button template message is parsed properly" $
            decodeSSm "buttonTemplate.json" bm
        
        it "receipt template message is parsed properly" $
            decodeSSm "receiptTemplate.json" rm
    
    describe "request serialization" $ do
        it "text message is serialized properly" $ 
            (decode $ encode tm :: Maybe SendTextMessageRequest) 
            `shouldBe` Right tm
        
        it "image message is serialized properly" $ 
            checkSSm im
        
        it "generic template message is serialized properly" $
            checkSSm gm
        
        it "button template message is serialized properly" $
            checkSSm bm
        
        it "receipt template message is serialized properly" $
            checkSSm rm
     
     where
         decodeSSm fName m = run (\l -> eitherDecode l :: Either String SendStructuredMessageRequest) fName `shouldBe` Right m
         checkSSm m = (eitherDecode $ encode m :: Either String SendStructuredMessageRequest) `shouldBe` Right m

-- TODO: Consider adding additional tests for corner cases and failures
