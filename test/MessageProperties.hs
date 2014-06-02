module MessageProperties where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Network.Linx.Gateway.Message
import Network.Linx.Gateway.Types

import Generators ()

prop_message :: Message -> Bool
prop_message message@(Message _ msgPayload) =
  let encodedMessage              = encode message
      (encHeader, encPayload)     = LBS.splitAt 8 encodedMessage
      Header msgType (Length len) = decode encHeader
      msgPayload'                 = decodeProtocolPayload msgType encPayload
  in msgPayload == msgPayload' 
     && (fromIntegral len) == (LBS.length encPayload)