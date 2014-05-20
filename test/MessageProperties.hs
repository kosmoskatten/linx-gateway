module MessageProperties where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Binary
import Data.Int
import qualified Data.ByteString.Lazy as LBS
import Test.QuickCheck hiding (Success)
import Network.Linx.Gateway.Message

instance Arbitrary Message where
  arbitrary = return mkInterfaceRequest

prop_message :: Message -> Bool
prop_message message@(Message _ msgPayload) =
  let encodedMessage              = encode message
      (encHeader, encPayload)     = LBS.splitAt 8 encodedMessage
      Header msgType (Length len) = decode encHeader
      msgPayload'                 = decodeProtocolPayload msgType encPayload
  in msgPayload == msgPayload' 
     && (fromIntegral len) == (LBS.length encPayload)