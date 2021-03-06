-- | The module is implementing the LINX signal concept, which is the
-- user level payload exchanged between two services.
module Network.Linx.Gateway.Signal
       ( Signal (..)
       , SignalSelector (..)
       , SigNo (..)
       , PayloadSize (..)
       , encode
       , decode
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary hiding (putList)
import Data.Binary.Get (getLazyByteString)
import Data.Binary.Put (putLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Linx.Gateway.Types
import Network.Linx.Gateway.BinaryInt32
import Network.Linx.Gateway.BinaryList

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

-- | A signal selector is used to filter which signals to expect when
-- calling receive. It can either be one of two specific filters -
-- AnySignal or Cancel - or a generic filter.
data SignalSelector =
    -- | Accept any kind of signal.
    AnySignal
    -- | Cancel the last receive.
  | Cancel
    -- | Accept any of the specified signals. The list must not be
    -- empty.
  | Sel { selection :: ![SigNo] }
  deriving (Eq, Show)

-- | PayloadSize instances.
instance PayloadSize Signal where
  payloadSize NoSignal         = Length 8
  payloadSize NumericSignal {} = Length 8
  payloadSize sig@Signal {}    =
    let len = LBS.length $ sigData sig
    in toLength $ 8 + len
       
instance PayloadSize SignalSelector where
  payloadSize AnySignal  = Length 8
  payloadSize Cancel     = Length 8
  payloadSize sel@Sel {} = 
    let len = length $ selection sel
    in toLength $ 8 + len * 4

-- | Binary instances.
instance Binary Signal where
  get                          = do
    len <- asInt <$> get
    case len of
      0 -> getInt32 >> return NoSignal
      4 -> NumericSignal <$> get
      _ -> Signal <$> get <*> getLazyByteString (len - 4)
  
  put NoSignal                 = putInt32 0 >> putInt32 0
  put (NumericSignal sigNo')   = putInt32 4 >> put sigNo'
  put (Signal sigNo' sigData') =
    let len = toLength $ LBS.length sigData' + 4
    in put len >> put sigNo' >> putLazyByteString sigData'
       
instance Binary SignalSelector where
  get = do
    len <- asInt <$> get
    case len :: Int32 of
      0 -> getInt32 >> return Cancel
      1 -> getInt32 >> return AnySignal
      _ -> get >>= \n -> Sel <$> getList n
        
  put AnySignal  = putInt32 1 >> putInt32 0
  put Cancel     = putInt32 0 >> putInt32 0
  put sel@Sel {}
   | selection sel == [] = error "Cannot encode empty selector list"
   | otherwise           = do
     let len   = length (selection sel)
         len'  = toLength (len + 1)
         len'' = toLength len
     put (len') >> put len'' >> putList (selection sel)
