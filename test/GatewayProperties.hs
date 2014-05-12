module GatewayProperties
       ( prop_messageCodes
       , prop_version
       , prop_endianess
       ) where

import Test.QuickCheck
import Network.Linx.Gateway

instance Arbitrary MessageCode where
  arbitrary = elements [ InterfaceRequest
                       , InterfaceReply
                       , CreateRequest
                       , CreateReply
                       , DestroyRequest
                       , DestroyReply
                       , SendRequest
                       , SendReply
                       , ReceiveRequest
                       , ReceiveReply
                       , HuntRequest
                       , HuntReply
                       , AttachRequest
                       , AttachReply
                       , DetachRequest
                       , DetachReply
                       , NameRequest
                       , NameReply
                       ]
              
instance Arbitrary Version where
  arbitrary = elements [ Version100 ]
  
instance Arbitrary Endianess where
  arbitrary = elements [ LittleEndian, BigEndian ]

prop_messageCodes :: MessageCode -> Bool
prop_messageCodes code = code == (decode . encode) code

prop_version :: Version -> Bool
prop_version version = version == (decode . encode) version

prop_endianess :: Endianess -> Bool
prop_endianess endianess = endianess == (decode . encode) endianess