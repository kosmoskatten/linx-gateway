module Network.Linx.Gateway.Signal
       ( Signal (..)
       , sigSize
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

data Signal =
    Signal { sigNo   :: !SigNo
           , sigData :: !LBS.ByteString }
  | NumericSignal { sigNo :: !SigNo }
  | NoSignal
  deriving (Eq, Show)

sigSize :: Signal -> Length
sigSize NoSignal         = Length 4
sigSize NumericSignal {} = Length 8
sigSize sig@Signal {}    =
  let len = LBS.length $ sigData sig
  in Length $ 8 + (fromIntegral len)

instance Binary Signal where
  get                          = do
    Length len <- get
    case len of
      0 -> return NoSignal
      4 -> NumericSignal <$> get
      _ -> Signal <$> get <*> getLazyByteString (fromIntegral len - 4)
  
  put NoSignal                 = putInt32 0
  put (NumericSignal sigNo')   = putInt32 4 >> put sigNo'
  put (Signal sigNo' sigData') =
    let len = Length (fromIntegral $ LBS.length sigData' + 4)
    in put len >> put sigNo' >> putLazyByteString sigData'