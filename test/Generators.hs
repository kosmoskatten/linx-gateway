module Generators where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Test.QuickCheck
import Network.Linx.Gateway.Message
import Network.Linx.Gateway.Signal
import Network.Linx.Gateway.Types

instance Arbitrary Signal where
  arbitrary = oneof [ pure NoSignal
                    , NumericSignal <$> arbitrary
                    , Signal <$> arbitrary <*> byteString ]
              
instance Arbitrary SignalSelector where
  arbitrary = frequency [ (1, pure AnySignal)
                        , (1, pure Cancel)
                        , (8, Sel <$> listOf1 arbitrary) ]

instance Arbitrary SigNo where
  arbitrary = SigNo <$> choose (1, maxBound)

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
              
instance Arbitrary CString where
  arbitrary = mkCString <$> string

instance Arbitrary Timeout where
  arbitrary = frequency [ (1, pure Infinity)
                        , (5, Timeout <$> choose (1, maxBound)) ]
              
instance Arbitrary Attref where
  arbitrary = Attref <$> choose (1, maxBound)

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
  arbitrary = frequency [ (4, interfaceRequest)
                        , (4, interfaceReply)
                        , (2, createRequest)
                        , (4, createReply)
                        , (2, destroyRequest)
                        , (1, destroyReply)
                        , (4, huntRequest)
                        , (2, huntReply)
                        , (4, receiveRequest)
                        , (4, receiveReply)
                        , (4, sendRequest)
                        , (1, sendReply)
                        , (4, attachRequest)
                        , (2, attachReply)
                        , (2, detachRequest)
                        , (1, detachReply)
                        , (1, nameRequest)
                        , (2, nameReply) ]
  
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

attachRequest :: Gen Message
attachRequest = mkAttachRequest <$> arbitrary <*> arbitrary

attachReply :: Gen Message
attachReply = mkAttachReply <$> arbitrary

detachRequest :: Gen Message
detachRequest = mkDetachRequest <$> arbitrary

detachReply :: Gen Message
detachReply = return mkDetachReply

nameRequest :: Gen Message
nameRequest = return mkNameRequest

nameReply :: Gen Message
nameReply = mkNameReply <$> string

clientName :: Gen String
clientName = string

string :: Gen String
string = listOf1 (elements $ ['a'..'z']++['A'..'Z'])

byteString :: Gen LBS.ByteString
byteString = LBS.pack <$> string