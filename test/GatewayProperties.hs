module GatewayProperties
       ( prop_messageCodes
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

prop_messageCodes :: MessageCode -> Bool
prop_messageCodes code = code == (decode . encode) code