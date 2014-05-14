module GatewayProperties where

import Control.Applicative ((<$>), (<*>))
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
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

instance Arbitrary ProtocolPayload where
  arbitrary = oneof [ InterfaceRequest <$> arbitrary <*> arbitrary
                    , interfaceReply
                    ]

instance Arbitrary Message where
  arbitrary = toMessage <$> arbitrary

interfaceReply :: Gen ProtocolPayload
interfaceReply = do
  status <- arbitrary
  version <- arbitrary
  flags <- arbitrary
  codes <- listOf arbitrary
  let len = fromIntegral $ length codes
  return $ InterfaceReply status version flags len codes

prop_message :: Message -> Bool
prop_message message@(Message _ size _) =
  let codedMessage = encode message
  in LBS.length codedMessage == fromIntegral (size + 8)
     && message == decode codedMessage
