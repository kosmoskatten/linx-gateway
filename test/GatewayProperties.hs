module GatewayProperties where

import Control.Applicative ((<$>), (<*>))
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Test.QuickCheck hiding (Success)
import Network.Linx.Gateway

instance Arbitrary Version where
  arbitrary = elements [ Version100 ]
  
instance Arbitrary Endianess where
  arbitrary = elements [ LittleEndian, BigEndian ]

instance Arbitrary Status where
  arbitrary = elements [ Error, Success ]

instance Arbitrary ProtocolPayload where
  arbitrary = oneof [InterfaceRequest <$> arbitrary <*> arbitrary]

instance Arbitrary Message where
  arbitrary = toMessage `fmap` arbitrary

prop_message :: Message -> Bool
prop_message message@(Message _ size _) =
  let codedMessage = encode message
  in LBS.length codedMessage == fromIntegral (size + 8)
     && message == decode codedMessage
