{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

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
    , TextMessage                  (..)
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import           Network.HTTP.Client.MultipartFormData
import           Network.Mime
import           Servant.Client.MultipartFormData (ToMultipartFormData (..))
import           Web.FBMessenger.API.Bot.JsonExt


-- | Informations about the recipient of the message
data Recipient = Recipient 
  { recipientphone_number :: Maybe Text  -- ^ Phone number of the recipient with the format +1(212)555-2368
  , recipientid           :: Maybe Text  -- ^ ID of recipient
  } deriving (Show, Generic)

instance ToJSON Recipient where
    toJSON = toJsonDrop 10

instance FromJSON Recipient where
    parseJSON = parseJsonDrop 10

-- | Take reciptient id (optional) or phone_number (optional) and return a Maybe Recipient object.
--   Return Nothing when values are either both (Just _) or both Nothing.  
recipient :: Maybe Text -> Maybe Text -> Maybe Recipient
recipient Nothing Nothing   = Nothing
recipient (Just _) (Just _) = Nothing
recipient rid phone_number  = pure $ Recipient rid phone_number 


-- | Push notification type for the message
data NotificationType = Regular        -- ^ will emit a sound/vibration and a phone notification (default)
                      | SilentPush     -- ^ will just emit a phone notification
                      | NoPush         -- ^ will not emit either
                      deriving Show

instance ToJSON NotificationType where
  toJSON Regular    = "REGULAR"
  toJSON SilentPush = "SILENT_PUSH"
  toJSON NoPush     = "NO_PUSH"

instance FromJSON NotificationType where
  parseJSON "REGULAR"     = pure Regular
  parseJSON "SILENT_PUSH" = pure SilentPush
  parseJSON "NO_PUSH"     = pure NoPush
  parseJSON _             = fail "Failed to parse NotificationType"


-- | Content of the message for a text-only message.
--   The message text must be UTF-8 and there is a limit of 320 characters
data TextMessage = TextMessage 
  { text_message_text :: Text       -- Message text, must be UTF-8, 320 character limit 
  } deriving (Show, Generic)

instance ToJSON TextMessage where
    toJSON = toJsonDrop 13

instance FromJSON TextMessage where
    parseJSON = parseJsonDrop 13

-- | This object represents a text message request
data SendTextMessageRequest = SendTextMessageRequest
  { message_recipient         :: Recipient
  , message_message           :: TextMessage
  , message_notification_type :: Maybe NotificationType
  } deriving (Show, Generic)

instance ToJSON SendTextMessageRequest where
  toJSON = toJsonDrop 8

instance FromJSON SendTextMessageRequest where
  parseJSON = parseJsonDrop 8

-- | Take a notification type (optional), a recipient and a text. Return a SendTextMessageRequest
sendTextMessageRequest :: Maybe NotificationType -> Recipient -> Text -> SendTextMessageRequest
sendTextMessageRequest notificationType recipient text = SendTextMessageRequest recipient text_message notificationType
  where text_message = TextMessage text


-- | Attachment for a structured message
data MessageAttachment = MessageAttachment
  { message_attachment_type    :: AttachmentType
  , message_attachment_payload :: AttachmentPayload
  } deriving (Show, Generic)

instance ToJSON MessageAttachment where
    toJSON = toJsonDrop 19

instance FromJSON MessageAttachment where
    parseJSON = parseJsonDrop 19

-- | Type of attachment for a structured message
data AttachmentType = AttachmentImage 
                    | AttachmentTemplate 
                    deriving Show

instance ToJSON AttachmentType where
  toJSON AttachmentImage    = "image"
  toJSON AttachmentTemplate = "template"

instance FromJSON AttachmentType where
  parseJSON "image"    = pure AttachmentImage
  parseJSON "template" = pure AttachmentTemplate
  parseJSON _          = fail "Failed to parse AttachmentType"
  
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
data AttachmentPayload = 
    ImagePayload { img_url :: Text } 
  | GenericTemplate 
    { gen_template_type  :: TemplateType              -- ^ Value must be "generic"
    , gen_elements       :: [BubbleElement]           -- ^ Data for each bubble in message 
    }
  | ButtonTemplate  
    { btn_template_type  :: TemplateType              -- ^ Value must be "button"
    , btn_text           :: Text                      -- ^ Text that appears in main body
    , btn_buttons        :: [Button]                  -- ^ Set of buttons that appear as call-to-actions
    }
  | ReceiptTemplate 
    { rcp_template_type  :: TemplateType              -- ^ Value should be "receipt"
    , rcp_recipientname :: Text                      -- ^ Recipient's Name
    , rcp_order_number   :: Text                      -- ^ Order number. Must be unique
    , rcp_currency       :: Text                      -- ^ Currency for order
    , rcp_payment_method :: Text                      -- ^ Payment method details. You may insert an arbitrary string but it is recommended to provide enough information for the person to decipher which payment method and account they used number)
    , rcp_timestamp      :: Maybe Text                -- ^ Timestamp of order
    , rcp_order_url      :: Maybe Text                -- ^ URL of order
    , rcp_elements       :: [ReceiptItem]             -- ^ Items in order       
    , rcp_address        :: Maybe ShippingAddress     -- ^ Shipping address. If you do not ship an item, you may omit the field
    , rcp_summary        :: PaymentSummary            -- ^ Payment summary
    , rcp_adjustment     :: Maybe [PaymentAdjustment] -- ^ Payment adjustments. Allow a way to insert adjusted pricing (e.g., sales)
    }
  | EmptyPayload {}                                   -- ^ Only used for multipart image messages 
  deriving (Show, Generic)

instance ToJSON AttachmentPayload where
    toJSON = toJsonDrop 4

instance FromJSON AttachmentPayload where
    parseJSON = parseJsonDrop 4

-- | Template type for structured messages
data TemplateType = GenericTType | ButtonTType | ReceiptTType deriving (Show)

instance ToJSON TemplateType where
  toJSON GenericTType = "generic"
  toJSON ButtonTType  = "button"
  toJSON ReceiptTType = "receipt"

instance FromJSON TemplateType where
  parseJSON "generic" = pure GenericTType
  parseJSON "button"  = pure ButtonTType
  parseJSON "receipt" = pure ReceiptTType
  parseJSON _         = fail "Failed to parse TemplateType"

-- | Type for Button objects. See Button type for additional informations.
data ButtonType = WebUrl | Postback deriving (Show)

instance ToJSON ButtonType where
  toJSON WebUrl    = "web_url"
  toJSON Postback  = "postback"

instance FromJSON ButtonType where
  parseJSON "web_url"  = pure WebUrl
  parseJSON "postback" = pure Postback
  parseJSON _          = fail "Failed to parse ButtonType"

-- | Button object for structured messages payloads
data Button = Button 
  { btn_type    :: ButtonType   -- ^ Value is "web_url" or "postback"
  , btn_title   :: Text         -- ^ Button title
  , btn_url     :: Maybe Text   -- ^ For web_url buttons, this URL is opened in a mobile browser when the button is tapped. Required if type is "web_url"
  , btn_payload :: Maybe Text   -- ^ For postback buttons, this data will be sent back to you via webhook. Required if type is "postback"
  } deriving (Show, Generic)

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
  } deriving (Show, Generic)

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
  } deriving (Show, Generic)

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
  } deriving (Show, Generic)

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
  } deriving (Show, Generic)

instance ToJSON PaymentSummary where
  toJSON = toJsonDrop 3
  
instance FromJSON PaymentSummary where
  parseJSON = parseJsonDrop 3
  
paymentSummary :: Double -> PaymentSummary
paymentSummary totalCost = PaymentSummary Nothing Nothing Nothing totalCost 

data PaymentAdjustment = PaymentAdjustment
  { pa_name   :: Maybe Text     -- ^ Name of adjustment
  , pa_amount :: Maybe Double   -- ^ Adjusted amount
  } deriving (Show, Generic)

instance ToJSON PaymentAdjustment where
  toJSON = toJsonDrop 3
  
instance FromJSON PaymentAdjustment where
  parseJSON = parseJsonDrop 3

-- | Content of the message for a structured message
data StructuredMessage = StructuredMessage 
  { structured_message_attachment :: MessageAttachment -- ^ Message text, must be UTF-8, 320 character limit 
  } deriving (Show, Generic)

instance ToJSON StructuredMessage where
    toJSON = toJsonDrop 19

instance FromJSON StructuredMessage where
    parseJSON = parseJsonDrop 19

-- | This object represents a structured message request
data SendStructuredMessageRequest = SendStructuredMessageRequest
  { structured_message_recipient         :: Recipient
  , structured_message_message           :: StructuredMessage         
  , structured_message_notification_type :: Maybe NotificationType
  } deriving (Show, Generic)

instance ToJSON SendStructuredMessageRequest where
  toJSON = toJsonDrop 19

instance FromJSON SendStructuredMessageRequest where
  parseJSON = parseJsonDrop 19

-- | Take a notification type (optional), a recipient, an image url.
--   Return a SendStructuredMessageRequest for a structured message with image attachment
sendImageMessageRequest :: Maybe NotificationType -> Recipient -> Text -> SendStructuredMessageRequest
sendImageMessageRequest notificationType recipient imgUrl = 
  SendStructuredMessageRequest recipient (structuredMessage attachment) notificationType
  where attachment = MessageAttachment AttachmentImage payload
        payload    = ImagePayload imgUrl

-- | Take a notification type (optional), a recipient, a list of ButtonElement.
--   Return a SendStructuredMessageRequest for a structured message with generic template
sendGenericTemplateMessageRequest :: Maybe NotificationType -> Recipient -> [BubbleElement]  -> SendStructuredMessageRequest
sendGenericTemplateMessageRequest notificationType recipient bubbles = 
  SendStructuredMessageRequest recipient (structuredMessage attachment) notificationType
  where attachment = MessageAttachment AttachmentTemplate payload
        payload    = GenericTemplate GenericTType bubbles 

-- | Take a notification type (optional), a recipient, the text of the message and a list of buttons (they will appear as call-to-actions).
--   Return a SendStructuredMessageRequest for a structured message with button template
sendButtonTemplateMessageRequest :: Maybe NotificationType -> Recipient -> Text -> [Button]  -> SendStructuredMessageRequest
sendButtonTemplateMessageRequest notificationType recipient text buttons = 
  SendStructuredMessageRequest recipient (structuredMessage attachment) notificationType
  where attachment = MessageAttachment AttachmentTemplate payload
        payload    = ButtonTemplate ButtonTType text buttons

-- | Take a notification type (optional), a recipient and all the informations needed to construct a ReceiptTemplate object.
--   Namely: the recipient name, the order number (must be unique), the currency, the payment method, the timestamp (optional), 
--   the order url (optional), a list with the receipt items, the shipping address (optional), the payment summary and, 
--   finally, a list of payment adjustments (optional).
--   Return a SendStructuredMessageRequest for a structured message with receipt template
sendReceiptTemplateMessageRequest :: Maybe NotificationType -> Recipient -> Text -> Text -> Text -> Text -> Maybe Text
                                           -> Maybe Text -> [ReceiptItem] -> Maybe ShippingAddress -> PaymentSummary -> Maybe [PaymentAdjustment]
                                           -> SendStructuredMessageRequest
sendReceiptTemplateMessageRequest notificationType recipient 
  recipientName orderNumber currency paymentMethod timeStamp orderUrl items address summary adjustments = 
    SendStructuredMessageRequest recipient (structuredMessage attachment) notificationType
    where 
        attachment = MessageAttachment AttachmentTemplate payload 
        payload    = ReceiptTemplate ReceiptTType recipientName orderNumber currency paymentMethod timeStamp orderUrl items address summary adjustments



-- | This object represents a Welcome Message (FromJSON is disabled for it)
data WelcomeMessage = 
    WelcomeTextMessageMessage { wtm_message :: TextMessage } 
  | WelcomeStructuredMessage { wsm_message :: StructuredMessage } 
  deriving (Show, Generic)
  
instance ToJSON WelcomeMessage where
  toJSON = toJsonDrop 4

--instance FromJSON WelcomeMessage where
--  parseJSON = parseJsonDrop 4 

-- | This object represents a Welcome Message request (FromJSON is disabled for it)
data WelcomeMessageRequest = WelcomeMessageRequest 
  { wm_setting_type    :: Text
  , wm_thread_state    :: Text
  , wm_call_to_actions :: [WelcomeMessage]
  } deriving (Show, Generic)

instance ToJSON WelcomeMessageRequest where
  toJSON = toJsonDrop 3
  
-- instance FromJSON WelcomeMessageRequest where
--   parseJSON = parseJsonDrop 3 

-- | Take a text. Return a WelcomeMessageRequest
setWelcomeTextMessageRequest :: Text -> WelcomeMessageRequest
setWelcomeTextMessageRequest text = WelcomeMessageRequest callToActions newThread [wm]
  where wm = WelcomeTextMessageMessage (TextMessage text) 

-- | Take an image url.
--   Return a WelcomeMessageRequest for a structured message with image attachment
setWelcomeImageMessageRequest :: Text -> WelcomeMessageRequest
setWelcomeImageMessageRequest imgUrl = WelcomeMessageRequest callToActions newThread [wm]
  where wm         = WelcomeStructuredMessage (structuredMessage attachment)
        attachment = MessageAttachment AttachmentImage payload
        payload    = ImagePayload imgUrl 

-- | Take a list of ButtonElement.
--   Return a WelcomeMessageRequest for a structured message with generic template
setWelcomeGenericTemplateMessageRequest :: [BubbleElement]  -> WelcomeMessageRequest
setWelcomeGenericTemplateMessageRequest bubbles = WelcomeMessageRequest callToActions newThread [wm]
  where wm         = WelcomeStructuredMessage (structuredMessage attachment)
        attachment = MessageAttachment AttachmentTemplate payload
        payload    = GenericTemplate GenericTType bubbles 

-- | Take the text of the message and a list of buttons (they will appear as call-to-actions).
--   Return a WelcomeMessageRequest for a structured message with button template
setWelcomeButtonTemplateMessageRequest :: Text -> [Button]  -> WelcomeMessageRequest
setWelcomeButtonTemplateMessageRequest text buttons = WelcomeMessageRequest callToActions newThread [wm]
  where wm         = WelcomeStructuredMessage (structuredMessage attachment)
        attachment = MessageAttachment AttachmentTemplate payload
        payload    = ButtonTemplate ButtonTType text buttons


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

-- | Return a FileUpload from a given FilePath. 
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
    immr_recipient                  :: Recipient        -- ^ Recipient user
  , immr_file_data                  :: payload           -- ^ Photo to send. Formats supported: jpg and png.
  , immr_message                    :: MessageAttachment -- ^ This MUST be MessageAttachment AttachmentImage EmptyPayload{}
  } deriving (Show, Generic)

instance ToJSON (UploadImageMessageRequest Text) where
  toJSON = toJsonDrop 5

instance FromJSON (UploadImageMessageRequest Text) where
  parseJSON = parseJsonDrop 5

-- sendImageMessageRequest :: Recipient -> Text -> UploadImageMessageRequest Text
-- sendImageMessageRequest reciptient image = UploadImageMessageRequest recipient image emptyImageMessageAttachment

-- | Take a recipient and FileUpload (relative to a jpg or png). Return a (UploadImageMessageRequest FileUpload) 
--   for a structured message contatining and image uploaded using multipart form data. 
uploadImageMessageRequest :: Recipient -> FileUpload -> UploadImageMessageRequest FileUpload
uploadImageMessageRequest recipient image = UploadImageMessageRequest recipient image emptyImageMessageAttachment

instance ToMultipartFormData (UploadImageMessageRequest FileUpload) where
  toMultipartFormData req =
    [ partLBS "recipient" . encode $ immr_recipient req
    , partLBS "message" $ encode $ immr_message req
    , fileUploadToPart "file_data" (immr_file_data req) ]


-- Helpers

structuredMessage :: MessageAttachment -> StructuredMessage
structuredMessage attachment = StructuredMessage{ structured_message_attachment = attachment }

emptyImageMessageAttachment :: MessageAttachment
emptyImageMessageAttachment =  MessageAttachment AttachmentImage EmptyPayload{}

callToActions :: Text
callToActions = T.pack "call_to_actions"
newThread :: Text
newThread = T.pack "new_thread"