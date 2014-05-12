module GatewayProperties
       ( prop_messageCodes
       ) where

import Test.QuickCheck
import Network.Linx.Gateway

instance Arbitrary MessageCode where
  arbitrary = elements [ InterfaceRequest
                       , InterfaceReply
                       ]

prop_messageCodes :: MessageCode -> Bool
prop_messageCodes code = code == (decode . encode) code