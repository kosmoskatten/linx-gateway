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
                    , receiveRequest
                    , receiveReply
                    ]

instance Arbitrary Length where
  arbitrary = Length <$> int32

instance Arbitrary CString where
  arbitrary = CString <$> neByteString

instance Arbitrary Pid where
  arbitrary = Pid <$> int32

instance Arbitrary SigNo where
  arbitrary = SigNo <$> int32
  
instance Arbitrary SigData where
  arbitrary = SigData <$> byteString

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
createRequest = CreateRequest <$> arbitrary <*> arbitrary

createReply :: Gen ProtocolPayload
createReply = CreateReply <$> arbitrary <*> arbitrary <*> arbitrary

destroyRequest :: Gen ProtocolPayload
destroyRequest = DestroyRequest <$> arbitrary

destroyReply :: Gen ProtocolPayload
destroyReply = DestroyReply <$> arbitrary

sendRequest :: Gen ProtocolPayload
sendRequest = 
  mkSendRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

sendReply :: Gen ProtocolPayload
sendReply = mkSendReply <$> arbitrary

receiveRequest :: Gen ProtocolPayload
receiveRequest = mkReceiveRequest <$> arbitrary <*> arbitrary

receiveReply :: Gen ProtocolPayload
receiveReply = do
  sigNo <- frequency [ (1, pure Nothing), (5, Just <$> arbitrary) ]
  case sigNo of
    Nothing -> mkReceiveReply <$> arbitrary <*> arbitrary  <*> arbitrary
                              <*> pure Nothing <*> pure Nothing
    _       -> mkReceiveReply <$> arbitrary <*> arbitrary <*> arbitrary
                              <*> pure sigNo <*> (Just <$> arbitrary)

neByteString :: Gen LBS.ByteString
neByteString = 
  LBS.pack <$> listOf1 (elements (['a'..'z']++['A'..'Z']++['0'..'9']))
  
byteString :: Gen LBS.ByteString
byteString = 
  LBS.pack <$> listOf (elements (['a'..'z']++['A'..'Z']++['0'..'9']))
  
int32 :: Gen Int32
int32 = choose (0, maxBound)

weightedLength :: Gen Length
weightedLength = Length <$> frequency [ (1, pure 0), (4, int32) ]

prop_message :: Message -> Bool
prop_message message@(Message _ (Length len) _) =
  let codedMessage = encode message
  in LBS.length codedMessage == fromIntegral (len + 8)
     && message == decode codedMessage
