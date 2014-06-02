module Network.Linx.Gateway.Signal
       ( Signal (..)
       , SigNo (..)
       , PayloadSize (..)
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

-- | Type class to determine the size of a signal.
class PayloadSize a where
  payloadSize :: a -> Length

-- | A signal - user lever payload data - is coded into three
-- different fields in the gateway protocol: Signal length in bytes,
-- including signal number. Signal number. Array of signal data. Even
-- in the case of 'NoSignal' the binary encoding is eight bytes.
data Signal =
    -- | A full bodied signal with number and data.
    Signal { sigNo   :: !SigNo
           , sigData :: !LBS.ByteString }
    
    -- | A signal data only is carrying a signal number, but no data.
  | NumericSignal { sigNo :: !SigNo }
    
    -- | An 'empty' signal.
  | NoSignal
  deriving (Eq, Show)

-- | PayloadSize instance.
instance PayloadSize Signal where
  payloadSize NoSignal         = Length 8
  payloadSize NumericSignal {} = Length 8
  payloadSize sig@Signal {}    =
    let len = LBS.length $ sigData sig
    in toLength $ 8 + len

-- | Binary instance.
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