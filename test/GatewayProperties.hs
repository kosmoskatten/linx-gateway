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

instance Arbitrary InterfaceRequest where
  arbitrary = InterfaceRequest <$> arbitrary <*> arbitrary

instance (Arbitrary a, Binary a, Payload a) => Arbitrary (Message a) where
  arbitrary = toMessage `fmap` arbitrary

data MyMaybe a = MyNothing | MyJust a
               deriving (Show, Eq)

instance (Arbitrary a, Num a) => Arbitrary (MyMaybe a) where
  arbitrary = MyJust `fmap` arbitrary

prop_messageCodes :: MessageCode -> Bool
prop_messageCodes code = code == (decode . encode) code

prop_version :: Version -> Bool
prop_version version = version == (decode . encode) version

prop_endianess :: Endianess -> Bool
prop_endianess endianess = endianess == (decode . encode) endianess

prop_status :: Status -> Bool
prop_status status = status == (decode . encode) status

prop_message :: Binary a => MessageCode -> Message a -> Bool
prop_message expectedCode message@(Message code size payload) =  
  let codedPayload = encode payload
  in expectedCode == code
     && LBS.length codedPayload == fromIntegral size
--     && message == (decode . encode) message

prop_messageInterfaceRequest :: Message InterfaceRequest -> Bool
prop_messageInterfaceRequest = prop_message InterfaceRequestOp