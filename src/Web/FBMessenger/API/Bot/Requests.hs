{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds  #-}

-- | This module contains data objects which represents requests to Messenger Platform Bot API
module Web.FBMessenger.API.Bot.Requests 
    ( -- * Types
      Button                       (..)
    , BubbleElement                (..)
    , FileUpload                   (..)
    , FileUploadContent            (..)
    , NotificationType             (..)
    , PaymentSummary               (..)
    , PaymentAdjustment            (..)
    , Recipient                    (..)
    , ReceiptItem                  (..)
    , ShippingAddress              (..)
    , SendTextMessageRequest       (..)
    , SendStructuredMessageRequest (..)
    , UploadImageMessageRequest    (..)
    , WelcomeMessageRequest        (..)
    -- * Functions
    , bubbleElement
    , localFileUpload
    , paymentSummary
    , postbackButton
    , receiptItem
    , recipient
    , shippingAddress
    , sendButtonTemplateMessageRequest
    , sendGenericTemplateMessageRequest
    , sendImageMessageRequest
    , sendReceiptTemplateMessageRequest
    , sendTextMessageRequest
    , setWelcomeButtonTemplateMessageRequest
    , setWelcomeGenericTemplateMessageRequest
    , setWelcomeImageMessageRequest
    , setWelcomeTextMessageRequest
    , uploadImageMessageRequest
    , webUrlButton
) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import           Network.HTTP.Client.MultipartFormData
import           Network.Mime
--import           Prelude hiding (lookup)
import           Servant.Client.MultipartFormData (ToMultipartFormData (..))
import           Web.FBMessenger.API.Bot.JsonExt


-- | Informations about the recipient of the message
data Recipient = Recipient 
  { recipient_id           :: Maybe Text  -- ^ ID of recipient
  , recipient_phone_number :: Maybe Text  -- ^ Phone number of the recipient with the format +1(212)555-2368
  } deriving (Eq, Show, Generic)

instance ToJSON Recipient where
    toJSON = toJsonDrop 10

instance FromJSON Recipient where
    parseJSON = parseJsonDrop 10

-- | Take reciptient id (optional) or phone_number (optional) and return a 'Maybe Recipient' object.
--   Return Nothing when values are either both (Just _) or both Nothing.  
recipient :: Maybe Text -> Maybe Text -> Maybe Recipient
recipient Nothing Nothing   = Nothing
recipient (Just _) (Just _) = Nothing
recipient rid phone_number  = pure $ Recipient rid phone_number 


-- | Push notification type for the message
data NotificationType = Regular        -- ^ will emit a sound/vibration and a phone notification (default)
                      | SilentPush     -- ^ will emit a phone notification
                      | NoPush         -- ^ will not emit either
                      deriving (Eq, Show)

instance ToJSON NotificationType where
  toJSON Regular    = "REGULAR"
  toJSON SilentPush = "SILENT_PUSH"
  toJSON NoPush     = "NO_PUSH"

instance FromJSON NotificationType where
  parseJSON "REGULAR"     = pure Regular
  parseJSON "SILENT_PUSH" = pure SilentPush
  parseJSON "NO_PUSH"     = pure NoPush
  parseJSON _             = fail "Failed to parse NotificationType"


-- | This object represents a text message request
--   The message text must be UTF-8, with 320 character limit
data SendTextMessageRequest = SendTextMessageRequest
  { mRecipient        :: Recipient
  , mText             :: Text
  , mNotificationType :: Maybe NotificationType
  } deriving (Eq, Show, Generic)

instance ToJSON SendTextMessageRequest where
  toJSON SendTextMessageRequest{..} = omitNulls [ "recipient" .= mRecipient, "message" .= tw, "notification_type" .= mNotificationType ]
     where tw = object [ "text" .= mText ]
  
instance FromJSON SendTextMessageRequest where
  parseJSON = withObject "send text message" $ \o ->
    let t = o .: "message" >>= (.: "text") in 
      SendTextMessageRequest <$> o .: "recipient" <*> t <*> o .:? "notification_type"
    
-- | Take a notification type (optional), a recipient and a text. 
--   Return a 'SendTextMessageRequest'. 
--   Raise an error if the text is longer than 320 characters.
sendTextMessageRequest :: Maybe NotificationType -> Recipient -> Text -> SendTextMessageRequest
sendTextMessageRequest notificationType recipient text 
  | T.length text <= 320 = SendTextMessageRequest recipient text notificationType
  | otherwise            = error "message text too long: the text must be UTF-8, with 320 character limit"
      
  
-- | Payload of attachment for structured messages.
--
--   Although not enforced in any way by this wrapper, the following generic limits are in place
--   (see also https://developers.facebook.com/docs/messenger-platform/send-api-reference#guidelines)
--
--       Title: 80 characters
--       Subtitle: 80 characters
--       Call-to-action title: 20 characters
--       Call-to-action items: 3 buttons
--       Bubbles per message (horizontal scroll): 10 elements
--       
--       Image ratio is 1.91:1
--
 
data ImagePayload = ImagePayload 
    { imgUrl :: Text                                -- ^ Image url 
    } deriving (Eq, Show)

parseImagePayload :: Object -> Parser ImagePayload
parseImagePayload v = ImagePayload <$> v.: "url"     
 
data GenericTemplate = GenericTemplate 
    { genElements       :: [BubbleElement]           -- ^ Data for each bubble in message 
    } deriving (Eq, Show)

parseGenericTemplate :: Object -> Parser GenericTemplate
parseGenericTemplate v = GenericTemplate <$> v .: "elements"

data ButtonTemplate = ButtonTemplate
    { btnText           :: Text                      -- ^ Text that appears in main body
    , btnButtons        :: [Button]                  -- ^ Set of buttons that appear as call-to-actions
    } deriving (Eq, Show)

parseButtonTemplate :: Object -> Parser ButtonTemplate
parseButtonTemplate v = ButtonTemplate <$> v .: "text" <*> v .: "buttons" 

data ReceiptTemplate = ReceiptTemplate 
    { rcpRecipientName   :: Text                      -- ^ Recipient's Name
    , rcpOrderNumber     :: Text                      -- ^ Order number. Must be unique
    , rcpCurrency        :: Text                      -- ^ Currency for order
    , rcpPaymentMethod   :: Text                      -- ^ Payment method details. You may insert an arbitrary string but it is recommended to provide enough information for the person to decipher which payment method and account they used number)
    , rcpTimestamp       :: Maybe Text                -- ^ Timestamp of order
    , rcpOrderUrl        :: Maybe Text                -- ^ URL of order
    , rcpElements        :: [ReceiptItem]             -- ^ Items in order       
    , rcpAddress         :: Maybe ShippingAddress     -- ^ Shipping address. If you do not ship an item, you may omit the field
    , rcpSummary         :: PaymentSummary            -- ^ Payment summary
    , rcpAdjustments     :: Maybe [PaymentAdjustment] -- ^ Payment adjustments. Allow a way to insert adjusted pricing (e.g., sales)
    } deriving (Eq, Show)

parseReceiptTemplate :: Object -> Parser ReceiptTemplate
parseReceiptTemplate v = ReceiptTemplate <$> v .: "recipient_name"
                                         <*> v .: "order_number"
                                         <*> v .: "currency"
                                         <*> v .: "payment_method"
                                         <*> v .:? "timestamp"
                                         <*> v .:? "order_url"
                                         <*> v .: "elements"
                                         <*> v .:? "address"
                                         <*> v .: "summary"
                                         <*> v .:? "adjustments" 

instance ToJSON ImagePayload where
    toJSON ImagePayload{..} = object [ "url" .= imgUrl ]
instance FromJSON ImagePayload where
    parseJSON = withObject "image payload" $ \v -> parseImagePayload v
    
instance ToJSON GenericTemplate where
    toJSON GenericTemplate{..} = object [ "template_type" .= ("generic"::String), "elements" .= genElements ]
instance FromJSON GenericTemplate where
    parseJSON = withObject "generic template payload" $ \v -> parseGenericTemplate v
    
instance ToJSON ButtonTemplate where
    toJSON ButtonTemplate{..} = object [ "template_type" .= ("button"::String), "text" .= btnText, "buttons" .= btnButtons ]
instance FromJSON ButtonTemplate where
    parseJSON = withObject "button template payload" $ \v -> parseButtonTemplate v
    
instance ToJSON ReceiptTemplate where
    toJSON ReceiptTemplate{..} = omitNulls [ "template_type"  .= ("receipt"::String)
                                           , "recipient_name" .= rcpRecipientName
                                           , "order_number"   .= rcpOrderNumber
                                           , "currency"       .= rcpCurrency
                                           , "payment_method" .= rcpPaymentMethod
                                           , "timestamp"      .= rcpTimestamp
                                           , "order_url"      .= rcpOrderUrl
                                           , "elements"       .= rcpElements
                                           , "address"        .= rcpAddress
                                           , "summary"        .= rcpSummary
                                           , "adjustments"    .= rcpAdjustments ]
instance FromJSON ReceiptTemplate where
    parseJSON = withObject "receipt template payload" $ \v -> parseReceiptTemplate v

data AttachmentWrapper = ItImage ImagePayload 
                       | ItGeneric GenericTemplate 
                       | ItButton ButtonTemplate 
                       | ItReceipt ReceiptTemplate deriving (Eq, Show)

instance ToJSON AttachmentWrapper where
    toJSON aw = object [ "type" .= t, "payload" .= p ]
      where 
        (t, p) = case aw of
                    ItImage a    -> ("image":: String,    toJSON a)
                    ItGeneric a  -> ("template":: String, toJSON a)
                    ItButton a   -> ("template":: String, toJSON a)
                    ItReceipt a  -> ("template":: String, toJSON a)
instance FromJSON AttachmentWrapper where
    parseJSON = withObject "attachment wrapper" $ \o -> do
      type_   <- (o .: "type") :: Parser String
      payload <- o .: "payload" 
      case type_ of
        "image"    -> ItImage <$> parseImagePayload payload
        "template" -> do
          templateType <- (o .: "payload" >>= (.: "template_type")) :: Parser String
          case templateType of
            "generic" -> ItGeneric <$> parseGenericTemplate payload
            "button"  -> ItButton  <$> parseButtonTemplate payload 
            "receipt" -> ItReceipt <$> parseReceiptTemplate payload
            _ -> fail "impossible to parse the template type"
        _  -> fail "impossible to parse the attachment wrapper type" 


-- | Type for 'Button' objects. See 'Button' type for additional informations.
data ButtonType = WebUrl | Postback deriving (Eq, Show)

instance ToJSON ButtonType where
  toJSON WebUrl    = "web_url"
  toJSON Postback  = "postback"

instance FromJSON ButtonType where
  parseJSON "web_url"  = pure WebUrl
  parseJSON "postback" = pure Postback
  parseJSON _          = fail "Failed to parse ButtonType"

-- | 'Button' object for structured messages payloads
data Button = Button 
  { btn_type    :: ButtonType   -- ^ Value is "web_url" or "postback"
  , btn_title   :: Text         -- ^ Button title
  , btn_url     :: Maybe Text   -- ^ For web_url buttons, this URL is opened in a mobile browser when the button is tapped. Required if type is "web_url"
  , btn_payload :: Maybe Text   -- ^ For postback buttons, this data will be sent back to you via webhook. Required if type is "postback"
  } deriving (Eq, Show, Generic)

instance ToJSON Button where
    toJSON = toJsonDrop 4
instance FromJSON Button where
  parseJSON = parseJsonDrop 4
  
-- | Take the button title and the button url (this URL is opened in a mobile browser when the button is tapped)
--   and return a "web_url" button
webUrlButton :: Text -> Text -> Button
webUrlButton title url = Button WebUrl title (Just url) Nothing

-- | Take the button title and the button payload (this data will be sent back to you via webhook)
--   and return a "postback" button  
postbackButton :: Text -> Text -> Button
postbackButton title payload = Button Postback title Nothing (Just payload)

-- | Bubble element object for structured messages payloads
data BubbleElement = BubbleElement
  { elm_title      :: Text           -- ^ Bubble title
  , elm_item_url   :: Maybe Text     -- ^ URL that is opened when bubble is tapped
  , elm_image_url  :: Maybe Text     -- ^ Bubble image
  , elm_subtitle   :: Maybe Text     -- ^ Bubble subtitle
  , elm_buttons    :: Maybe [Button] -- ^ Set of buttons that appear as call-to-actions
  } deriving (Eq, Show, Generic)

instance ToJSON BubbleElement where
    toJSON = toJsonDrop 4
instance FromJSON BubbleElement where
    parseJSON = parseJsonDrop 4

-- | Take the bubble element title. The buttons will appear as call-to-action in Messenger.
--   Return a bubble Element
bubbleElement :: Text -> BubbleElement
bubbleElement title = BubbleElement title Nothing Nothing Nothing Nothing

data ReceiptItem = ReceiptItem 
  { re_title     :: Text         -- ^ Title of item
  , re_subtitle  :: Maybe Text   -- ^ Subtitle of item
  , re_quantity  :: Maybe Int    -- ^ Quantity of item
  , re_price     :: Maybe Int    -- ^ Item price
  , re_currency  :: Maybe Text   -- ^ Currency of price
  , re_image_url :: Maybe Text   -- ^ Image URL of item
  } deriving (Eq, Show, Generic)

instance ToJSON ReceiptItem where
  toJSON = toJsonDrop 3
instance FromJSON ReceiptItem where
  parseJSON = parseJsonDrop 3 

receiptItem :: Text -> ReceiptItem
receiptItem title = ReceiptItem title Nothing Nothing Nothing Nothing Nothing 

-- | Shipping address object for Receipt Template messages
data ShippingAddress = ShippingAddress 
  { sa_street_1    :: Text       -- ^ Street Address, line 1
  , sa_street_2    :: Maybe Text -- ^ Street Address, line 2
  , sa_city        :: Text       -- ^ City
  , sa_postal_code :: Text       -- ^ Postal Code
  , sa_state       :: Text       -- ^ State abbrevation
  , sa_country     :: Text       -- ^ Two-letter country abbreviation
  } deriving (Eq, Show, Generic)

instance ToJSON ShippingAddress where
  toJSON = toJsonDrop 3
instance FromJSON ShippingAddress where
  parseJSON = parseJsonDrop 3

shippingAddress :: Text -> Text -> Text -> Text -> Text -> ShippingAddress
shippingAddress street city postalCode state country = ShippingAddress street Nothing city postalCode state country 

-- | Payment summary object for Receipt Template messages
data PaymentSummary = PaymentSummary 
  { ps_subtotal      :: Maybe Double  -- ^ Subtotal
  , ps_shipping_cost :: Maybe Double  -- ^ Shipping Cost
  , ps_total_tax     :: Maybe Double  -- ^ Total Tax
  , ps_total_cost    :: Double        -- ^ Total Cost
  } deriving (Eq, Show, Generic)

instance ToJSON PaymentSummary where
  toJSON = toJsonDrop 3
instance FromJSON PaymentSummary where
  parseJSON = parseJsonDrop 3
  
paymentSummary :: Double -> PaymentSummary
paymentSummary totalCost = PaymentSummary Nothing Nothing Nothing totalCost

data PaymentAdjustment = PaymentAdjustment
  { pa_name   :: Maybe Text     -- ^ Name of adjustment
  , pa_amount :: Maybe Double   -- ^ Adjusted amount
  } deriving (Eq, Show, Generic)

instance ToJSON PaymentAdjustment where
  toJSON = toJsonDrop 3  
instance FromJSON PaymentAdjustment where
  parseJSON = parseJsonDrop 3


-- | This object represents a structured message request
data SendStructuredMessageRequest = SendStructuredMessageRequest
  { smRecipient         :: Recipient
  , smAttachment        :: AttachmentWrapper         
  , smNotificationType  :: Maybe NotificationType
  } deriving (Eq, Show)

instance ToJSON SendStructuredMessageRequest where
  toJSON SendStructuredMessageRequest{..} = omitNulls [ "recipient" .= smRecipient, "message" .= tw, "notification_type" .= smNotificationType ]
     where tw = object [ "attachment" .= smAttachment ]
  
instance FromJSON SendStructuredMessageRequest where
  parseJSON = withObject "send structured message" $ \o ->
    let aw = o .: "message" >>= (.: "attachment") in 
      SendStructuredMessageRequest <$> o .: "recipient" <*> aw <*> o .:? "notification_type"
 
-- | Take a notification type (optional), a recipient, an image url.
--   Return a 'SendStructuredMessageRequest' for a structured message with image attachment
sendImageMessageRequest :: Maybe NotificationType -> Recipient -> Text -> SendStructuredMessageRequest
sendImageMessageRequest notificationType recipient imgUrl = 
  SendStructuredMessageRequest recipient attachment notificationType
  where attachment = ItImage $ ImagePayload imgUrl

-- | Take a notification type (optional), a recipient, a list of 'ButtonElement'.
--   Return a 'SendStructuredMessageRequest' for a structured message with generic template
sendGenericTemplateMessageRequest :: Maybe NotificationType -> Recipient -> [BubbleElement]  -> SendStructuredMessageRequest
sendGenericTemplateMessageRequest notificationType recipient bubbles = 
  SendStructuredMessageRequest recipient attachment notificationType
  where attachment = ItGeneric $ GenericTemplate bubbles 

-- | Take a notification type (optional), a recipient, the text of the message and a list of 
--   buttons (they will appear as call-to-actions).
--   Return a 'SendStructuredMessageRequest' for a structured message with button template
sendButtonTemplateMessageRequest :: Maybe NotificationType -> Recipient -> Text -> [Button]  -> SendStructuredMessageRequest
sendButtonTemplateMessageRequest notificationType recipient text buttons = 
  SendStructuredMessageRequest recipient attachment notificationType
  where attachment = ItButton $ ButtonTemplate text buttons

-- | Take a notification type (optional), a recipient and all the informations needed to construct a 'ReceiptTemplate' object.
--   Namely: the recipient name, the order number (must be unique), the currency, the payment method, the timestamp (optional), 
--   the order url (optional), a list with the receipt items, the shipping address (optional), the payment summary and, 
--   finally, a list of payment adjustments (optional).
--   Return a 'SendStructuredMessageRequest' for a structured message with receipt template
sendReceiptTemplateMessageRequest :: Maybe NotificationType -> Recipient -> Text -> Text -> Text -> Text -> Maybe Text
                                           -> Maybe Text -> [ReceiptItem] -> Maybe ShippingAddress -> PaymentSummary -> Maybe [PaymentAdjustment]
                                           -> SendStructuredMessageRequest
sendReceiptTemplateMessageRequest notificationType recipient 
  recipientName orderNumber currency paymentMethod timeStamp orderUrl items address summary adjustments = 
    SendStructuredMessageRequest recipient attachment notificationType
    where 
        attachment = ItReceipt $ ReceiptTemplate recipientName orderNumber currency paymentMethod timeStamp orderUrl items address summary adjustments

-- | This object represents a Welcome Message (FromJSON is disabled for it)
data WelcomeMessageRequest = 
    WelcomeTextMessage        { wtmMessage :: Text } 
  | WelcomeStructuredMessage  { wsmMessage :: AttachmentWrapper } 
  | WelcomeEmptyMessage
  deriving (Eq, Show, Generic)
  
instance ToJSON WelcomeMessageRequest where
  toJSON mw = object [ "setting_type" .= ("call_to_actions"::String), "thread_state" .= ("new_thread"::String), "call_to_actions" .= at ]
    where at = case mw of
                  WelcomeTextMessage{..} -> [ object [ "message" .= object [ "text" .= wtmMessage ] ] ]
                  WelcomeStructuredMessage{..} -> [ object [ "message" .= wsmMessage ] ]
                  WelcomeEmptyMessage -> [ ]

--instance FromJSON WelcomeMessage where
--  parseJSON = parseJsonDrop 4 

-- | Take a text. Return a WelcomeMessageRequest
setWelcomeTextMessageRequest :: Text -> WelcomeMessageRequest
setWelcomeTextMessageRequest = WelcomeTextMessage 

-- | Take an image url.
--   Return a 'WelcomeMessageRequest' for a structured message with image attachment
setWelcomeImageMessageRequest :: Text -> WelcomeMessageRequest
setWelcomeImageMessageRequest imgUrl = WelcomeStructuredMessage attachment
  where attachment = ItImage $ ImagePayload imgUrl 

-- | Take a list of ButtonElement.
--   Return a 'WelcomeMessageRequest' for a structured message with generic template
setWelcomeGenericTemplateMessageRequest :: [BubbleElement]  -> WelcomeMessageRequest
setWelcomeGenericTemplateMessageRequest bubbles = WelcomeStructuredMessage attachment
  where attachment = ItGeneric $ GenericTemplate bubbles 

-- | Take the text of the message and a list of buttons (they will appear as call-to-actions).
--   Return a 'WelcomeMessageRequest' for a structured message with button template
setWelcomeButtonTemplateMessageRequest :: Text -> [Button]  -> WelcomeMessageRequest
setWelcomeButtonTemplateMessageRequest text buttons = WelcomeStructuredMessage attachment
  where attachment = ItButton $ ButtonTemplate text buttons


-- The code below is partially from https://github.com/klappvisor/haskell-telegram-api/blob/master/src/Web/Telegram/API/Bot/Requests.hs
-- Code Licenseded under BSD3 - Copyright Alexey Rodiontsev (c) 2015

-- | This object represents data (image, video, ...) to upload.
data FileUploadContent =
    FileUploadFile FilePath
  | FileUploadBS BS.ByteString
  | FileUploadLBS LBS.ByteString

-- | This object represents data (image, video, ...) with mime type to upload.
data FileUpload = FileUpload
  { fileUpload_type    :: Maybe MimeType    -- ^ Mime type of the upload.
  , fileUpload_content :: FileUploadContent -- ^ The payload/source to upload.
  }

-- | Return a 'FileUpload' from a given 'FilePath'. 
--   At the moment, only png and jpg images are supported by the API.
localFileUpload :: FilePath -> FileUpload
localFileUpload path = FileUpload
  { fileUpload_type    = Nothing
  , fileUpload_content = FileUploadFile path
  }

fileUploadToPart :: Text -> FileUpload -> Part
fileUploadToPart inputName fileUpload =
  let part =
        case fileUpload_content fileUpload of
          FileUploadFile path -> partFileSource inputName path
          FileUploadBS bs     -> partBS inputName bs
          FileUploadLBS lbs   -> partLBS inputName lbs
  in part { partContentType = fileUpload_type fileUpload }

utf8Part :: Text -> Text -> Part
utf8Part inputName = partBS inputName . T.encodeUtf8

-- | This object represents request for 'sendImage'
data UploadImageMessageRequest payload = UploadImageMessageRequest
  {
    uiRecipient                 :: Recipient        -- ^ Recipient user
  , uiFileData                  :: payload           -- ^ Photo to send. Formats supported: jpg and png.
  } deriving (Eq, Show)

instance ToJSON (UploadImageMessageRequest Text) where
  toJSON UploadImageMessageRequest{..} = object [ "recipient"  .= uiRecipient, "file_data"  .= uiFileData ]

instance FromJSON (UploadImageMessageRequest Text) where
  parseJSON = withObject "upload image message" $ \o -> do
    uiRecipient <- o .: "recipient"
    uiFileData  <- o .: "file_data"
    return UploadImageMessageRequest{..}

-- sendImageMessageRequest :: Recipient -> Text -> UploadImageMessageRequest Text
-- sendImageMessageRequest reciptient image = UploadImageMessageRequest recipient image emptyImageMessageAttachment

-- | Take a 'Recipient' and 'FileUpload' (relative to a jpg or png). Return a ('UploadImageMessageRequest FileUpload') 
--   for a structured message contatining and image uploaded using multipart form data. 
uploadImageMessageRequest :: Recipient -> FileUpload -> UploadImageMessageRequest FileUpload
uploadImageMessageRequest = UploadImageMessageRequest

instance ToMultipartFormData (UploadImageMessageRequest FileUpload) where
  toMultipartFormData req =
    [ partLBS "recipient" . encode $ uiRecipient req
    , partLBS "message"   $ encode $ object ["attachment" .= object ["type" .= ("image"::String), "payload" .= object [] ]]
    , fileUploadToPart "file_data" (uiFileData req) ]


-- Helpers

-- from http://bitemyapp.com/posts/2014-07-31-aeson-with-uncertainty-revised.html
omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull where
  notNull (_, Null) = False
  notNull _         = True
