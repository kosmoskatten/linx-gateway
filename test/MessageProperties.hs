module MessageProperties where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Binary
import Data.Int
import qualified Data.ByteString.Lazy as LBS
import Test.QuickCheck hiding (Success)
import Network.Linx.Gateway.Message

instance Arbitrary Version where
  arbitrary = oneof [ pure V100, Version <$> choose (1, 99) ]

instance Arbitrary Flags where
  arbitrary = oneof [ pure BigEndian
                    , pure LittleEndian
                    , Flags <$> choose (2, 99) ]
              
instance Arbitrary PayloadType where
  arbitrary = elements [ InterfaceRequestOp
                       , InterfaceReplyOp
                       , CreateRequestOp
                       , CreateReplyOp
                       , DestroyRequestOp
                       , DestroyReplyOp
                       , SendRequestOp
                       , SendReplyOp
                       , ReceiveRequestOp
                       , ReceiveReplyOp
                       , HuntRequestOp
                       , HuntReplyOp
                       , AttachRequestOp
                       , AttachReplyOp
                       , DetachRequestOp
                       , DetachReplyOp
                       , NameRequestOp
                       , NameReplyOp ]

instance Arbitrary Message where
  arbitrary = oneof [ interfaceRequest
                    , interfaceReply
                    , createRequest ]
  
interfaceRequest :: Gen Message
interfaceRequest = mkInterfaceRequest <$> arbitrary <*> arbitrary

interfaceReply :: Gen Message
interfaceReply = 
  mkInterfaceReply <$> arbitrary <*> arbitrary <*> (listOf arbitrary)
  
createRequest :: Gen Message
createRequest = mkCreateRequest <$> clientName

clientName :: Gen String
clientName = listOf1 (elements $ ['a'..'z']++['A'..'Z'])

prop_message :: Message -> Bool
prop_message message@(Message _ msgPayload) =
  let encodedMessage              = encode message
      (encHeader, encPayload)     = LBS.splitAt 8 encodedMessage
      Header msgType (Length len) = decode encHeader
      msgPayload'                 = decodeProtocolPayload msgType encPayload
  in msgPayload == msgPayload' 
     && (fromIntegral len) == (LBS.length encPayload)