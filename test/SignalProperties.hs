module SignalProperties where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Test.QuickCheck
import Network.Linx.Gateway.Signal
import Network.Linx.Gateway.Types

instance Arbitrary Signal where
  arbitrary = oneof [ pure NoSignal
                    , NumericSignal <$> sigNo'
                    , Signal <$> sigNo' <*> byteString ]
              
byteString :: Gen LBS.ByteString
byteString = LBS.pack <$> listOf1 (elements ['a'..'z'])

sigNo' :: Gen SigNo
sigNo' = SigNo <$> choose (1, 1000000)

prop_signal :: Signal -> Bool
prop_signal sig =
  let encodedSignal = encode sig
      Length len    = payloadSize sig
  in (fromIntegral len) == LBS.length encodedSignal
     && sig == decode encodedSignal