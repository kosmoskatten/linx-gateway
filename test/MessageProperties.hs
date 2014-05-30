module MessageProperties where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Test.QuickCheck hiding (Success)
import Network.Linx.Gateway.Message
import Network.Linx.Gateway.Types

import SignalProperties ()

instance Arbitrary Version where
  arbitrary = oneof [ pure V100, Version <$> choose (1, 99) ]

instance Arbitrary Flags where
  arbitrary = oneof [ pure BigEndian
                    , pure LittleEndian
                    , Flags <$> choose (2, 99) ]
              
instance Arbitrary Length where
  arbitrary = Length <$> choose (1, maxBound)

instance Arbitrary Pid where
  arbitrary = Pid <$> choose (1, maxBound)
              
instance Arbitrary SigNo where
  arbitrary = SigNo <$> choose (1, maxBound)

instance Arbitrary CString where
  arbitrary = mkCString <$> string

instance Arbitrary Timeout where
  arbitrary = frequency [ (1, pure Infinity)
                        , (5, Timeout <$> choose (1, maxBound)) ]

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
                    , createRequest 
                    , createReply 
                    , destroyRequest
                    , destroyReply 
                    , huntRequest 
                    , huntReply 
                    , receiveRequest 
                    , receiveReply 
                    , sendRequest 
                    , sendReply ]
  
interfaceRequest :: Gen Message
interfaceRequest = mkInterfaceRequest <$> arbitrary <*> arbitrary

interfaceReply :: Gen Message
interfaceReply = 
  mkInterfaceReply <$> arbitrary <*> arbitrary <*> (listOf arbitrary)
  
createRequest :: Gen Message
createRequest = mkCreateRequest <$> clientName

createReply :: Gen Message
createReply = mkCreateReply <$> arbitrary <*> arbitrary

destroyRequest :: Gen Message
destroyRequest = mkDestroyRequest <$> arbitrary

destroyReply :: Gen Message
destroyReply = return mkDestroyReply

huntRequest :: Gen Message
huntRequest = mkHuntRequest <$> arbitrary <*> arbitrary

huntReply :: Gen Message
huntReply = mkHuntReply <$> arbitrary

receiveRequest :: Gen Message
receiveRequest = mkReceiveRequest <$> arbitrary <*> arbitrary

receiveReply :: Gen Message
receiveReply = mkReceiveReply <$> arbitrary <*> arbitrary <*> arbitrary

sendRequest :: Gen Message
sendRequest = mkSendRequest <$> arbitrary <*> arbitrary <*> arbitrary

sendReply :: Gen Message
sendReply = return mkSendReply

clientName :: Gen String
clientName = string

string :: Gen String
string = listOf1 (elements $ ['a'..'z']++['A'..'Z'])

prop_message :: Message -> Bool
prop_message message@(Message _ msgPayload) =
  let encodedMessage              = encode message
      (encHeader, encPayload)     = LBS.splitAt 8 encodedMessage
      Header msgType (Length len) = decode encHeader
      msgPayload'                 = decodeProtocolPayload msgType encPayload
  in msgPayload == msgPayload' 
     && (fromIntegral len) == (LBS.length encPayload)