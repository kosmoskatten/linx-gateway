module SignalProperties where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.Linx.Gateway.Signal
import Network.Linx.Gateway.Types

import Generators ()

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
     
