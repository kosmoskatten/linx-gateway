module SignalProperties where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Test.QuickCheck
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

byteString :: Gen LBS.ByteString
byteString = LBS.pack <$> listOf1 (elements ['a'..'z'])

prop_signal :: Signal -> Bool
prop_signal sig =
  let encodedSignal = encode sig
      len           = payloadSize sig
  in (asInt len) == LBS.length encodedSignal
     && sig == decode encodedSignal
     
prop_signalSelector :: SignalSelector -> Bool
prop_signalSelector selector =
  let encodedSelector = encode selector
      len             = payloadSize selector
  in (asInt len) == LBS.length encodedSelector
     && selector == decode encodedSelector
     
