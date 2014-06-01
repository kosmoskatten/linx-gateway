module Network.Linx.Gateway.Signal
       ( Signal (..)
       , SigNo (..)
       , payloadSize
       , encode
       , decode
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary
import Data.Binary.Get (getLazyByteString)
import Data.Binary.Put (putLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Linx.Gateway.Types
import Network.Linx.Gateway.BinaryInt32

-- | A signal - user lever payload data - is coded into three
-- different fields in the gateway protocol:
-- + signal length in bytes, including signal number +
-- + signal number                                   +
-- + first four bytes of the signal data             +
data Signal =
    -- | A full bodied signal with number and data.
    Signal { sigNo   :: !SigNo
           , sigData :: !LBS.ByteString }
    
    -- | A signal data only is carrying a signal number, but no data.
  | NumericSignal { sigNo :: !SigNo }
    
    -- | An 'empty' signal.
  | NoSignal
  deriving (Eq, Show)

-- | The payload size for a signal in this implementation also
-- includes the signal length field, so the minimal coded size for a
-- signal is eight bytes.
payloadSize :: Signal -> Length
payloadSize NoSignal         = Length 8
payloadSize NumericSignal {} = Length 8
payloadSize sig@Signal {}    =
  let len = LBS.length $ sigData sig
  in toLength $ 8 + len

instance Binary Signal where
  get                          = do
    len <- asInt <$> get
    case len of
      0 -> getInt32 >>= \_ -> return NoSignal
      4 -> NumericSignal <$> get
      _ -> Signal <$> get <*> getLazyByteString (len - 4)
  
  put NoSignal                 = putInt32 0 >> putInt32 0
  put (NumericSignal sigNo')   = putInt32 4 >> put sigNo'
  put (Signal sigNo' sigData') =
    let len = toLength $ LBS.length sigData' + 4
    in put len >> put sigNo' >> putLazyByteString sigData'       