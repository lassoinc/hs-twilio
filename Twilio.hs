{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Twilio (
	Credentials(..),
	TwilioData(..),
	twilio,
	availableNumbers,
	buyNumber,
        repXml,
        xml
	) where
import Network.HTTP.Conduit
import Data.Aeson
import qualified Data.Map as Map
import Text.Blaze
import Text.Blaze.Renderer.Text
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)
import Control.Monad
import Data.Conduit
import Data.Monoid
import Data.ByteString (ByteString)
import Network.HTTP.Types
import qualified Data.ByteString.UTF8 as U8
import Yesod.Core (RepXml(..), toContent)
import Text.Hamlet.XML
-- import Text.XML.Xml2Html ()
import Text.XML

data TwilioData = TwilioData {
	twilioManager :: Manager,
	twilioRequest :: Request (ResourceT IO)
}

data Credentials = Credentials {
	account :: ByteString,
	token :: ByteString
} deriving (Show, Eq, Read)
$(deriveJSON id ''Credentials)

twilio :: Manager -> Credentials -> IO TwilioData
twilio manager creds = do
	req <- parseUrl $ "https://api.twilio.com/2010-04-01/Accounts/" ++ U8.toString (account creds)
	return (TwilioData manager (applyBasicAuth (account creds) (token creds) req))


twilioReq :: FromJSON a => ByteString -> [(ByteString, ByteString)] -> Bool -> TwilioData -> IO a
twilioReq p params post d
	= runResourceT $ do
		res <- httpLbs req'' (twilioManager d)
		case (decode . responseBody) res of
			Nothing -> error "Could not parse Twilio JSON"
			Just x -> return x
	where
		req' = (twilioRequest d) {path = path (twilioRequest d) `mappend` p }
		ascii = renderSimpleQuery True params
		req'' = if post then urlEncodedBody params req' else req' {queryString = ascii}


newtype AvailableNumbers = AvailabeNumbers { unAvailableNumbers :: [ByteString] }
instance FromJSON AvailableNumbers where
	parseJSON (Object o) = liftM AvailabeNumbers $ (o .: "available_phone_numbers") >>= mapM getNum
	parseJSON _ = mzero

getNum :: Value -> Parser ByteString
getNum (Object o) = o .: "phone_number"
getNum _ = mzero

-- | Get numbers available in the US
availableNumbers :: [(ByteString, ByteString)] -> TwilioData -> IO [ByteString]
availableNumbers p = liftM unAvailableNumbers . twilioReq  "/AvailablePhoneNumbers/US/Local.json" p False

-- | Buy numbers in the US
buyNumber :: ByteString -> TwilioData -> IO ()
buyNumber p = twilioReq "/IncomingPhoneNumbers" [("PhoneNumber", p)] True

-- | Wrap a quasiquoted bunch of xml in a Response element
repXml :: [Node] -> RepXml
repXml = RepXml . toContent . renderMarkup . toMarkup .
   flip (Document (Prologue [] Nothing [])) [] . Element "Response" Map.empty
