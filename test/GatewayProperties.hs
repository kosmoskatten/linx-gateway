module GatewayProperties where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Binary
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as LBS
import Test.QuickCheck hiding (Success)
import Network.Linx.Gateway

instance Arbitrary MessageCode where
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
                       , NameReplyOp
                       ]

instance Arbitrary Version where
  arbitrary = elements [ Version100 ]
  
instance Arbitrary Endianess where
  arbitrary = elements [ LittleEndian, BigEndian ]

instance Arbitrary Status where
  arbitrary = elements [ Error, Success ]

instance Arbitrary User where
  arbitrary = return AlwaysZero

instance Arbitrary Timeout where
  arbitrary = oneof [ pure Infinity
                    , Wait <$> int32
                    ]

instance Arbitrary ProtocolPayload where
  arbitrary = oneof [ interfaceRequest
                    , interfaceReply
                    , createRequest
                    , createReply
                    , destroyRequest
                    , destroyReply
                    , sendRequest
                    , sendReply
                    ]

instance Arbitrary Message where
  arbitrary = toMessage <$> arbitrary

interfaceRequest :: Gen ProtocolPayload
interfaceRequest = InterfaceRequest <$> arbitrary <*> arbitrary

interfaceReply :: Gen ProtocolPayload
interfaceReply = do
  status <- arbitrary
  version <- arbitrary
  flags <- arbitrary
  codes <- listOf arbitrary
  let len = Length (fromIntegral $ length codes)
  return $ InterfaceReply status version flags len codes

createRequest :: Gen ProtocolPayload
createRequest = CreateRequest <$> arbitrary <*> byteString

createReply :: Gen ProtocolPayload
createReply = CreateReply <$> arbitrary <*> int32 <*> int32

destroyRequest :: Gen ProtocolPayload
destroyRequest = DestroyRequest <$> int32

destroyReply :: Gen ProtocolPayload
destroyReply = DestroyReply <$> arbitrary

sendRequest :: Gen ProtocolPayload
sendRequest = mkSendRequest <$> int32 <*> int32 <*> int32 <*> byteString

sendReply :: Gen ProtocolPayload
sendReply = mkSendReply <$> arbitrary

byteString :: Gen LBS.ByteString
byteString = 
  LBS.pack <$> listOf1 (elements (['a'..'z']++['A'..'Z']++['0'..'9']))
  
int32 :: Gen Int32
int32 = choose (0, maxBound)

prop_message :: Message -> Bool
prop_message message@(Message _ size _) =
  let codedMessage = encode message
  in LBS.length codedMessage == fromIntegral (size + 8)
     && message == decode codedMessage
