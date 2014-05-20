{-# LANGUAGE DeriveGeneric #-}
module Network.Linx.Gateway.Message 
       ( Message (..)
       , Header (..)
       , ProtocolPayload
       , Length (..)
       , Version (..)
       , Flags (..)
       , mkInterfaceRequest
       , decodeProtocolPayload
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary
import Data.Binary.Get (runGet)
import Data.Int
import GHC.Generics
import qualified Data.ByteString.Lazy as LBS

-- | Message.
data Message =
  Message !Header !ProtocolPayload
  deriving (Show, Eq)

-- | Message header.
data Header =
  Header !PayloadType !Length
  deriving (Show, Eq, Generic)

-- | Protocol payload.
data ProtocolPayload =
    FailedRequest
  | InterfaceRequest !Version !Flags
  deriving (Show, Eq)

-- | Payload type discriminator.
data PayloadType =
    InterfaceRequestOp
  | InterfaceReplyOp
  deriving (Show, Eq)
           
-- | Length descriptor.
newtype Length = Length Int32
  deriving (Show, Eq, Generic)
           
-- | Version descriptor.
data Version =
    V100
  | Version !Int32
  deriving (Show, Eq)
           
-- | Flags descriptor.
data Flags =
    BigEndian
  | LittleEndian
  | Flags !Int32
  deriving (Show, Eq)

-- | Payload class. To be implemented by ProtocolPayload.
class Payload a where
  header :: a -> Header

instance Payload ProtocolPayload where
  header FailedRequest       = error "Shall not be called this way"
  header InterfaceRequest {} = Header InterfaceRequestOp (Length 8)

-- | Generic binary instances.
instance Binary Header
instance Binary Length

-- | Binary instance for 'Message'.
instance Binary Message where
  get = error "Shall not be called this way"
  put (Message msgHeader msgPayload) = put msgHeader >> put msgPayload

-- | Binary instance for 'ProtocolPayload'.
instance Binary ProtocolPayload where
  get = error "Shall not be called this way"
  put FailedRequest = error "Shall not be called this way"
  put (InterfaceRequest version flags) = put version >> put flags

-- | Binary instance for 'PayloadType'.
instance Binary PayloadType where
  get = do
    value <- getInt32
    return $
      case value of
        1 -> InterfaceRequestOp
        2 -> InterfaceReplyOp
        _ -> error "Unexpected discriminator code"
  
  put InterfaceRequestOp = putInt32 1
  put InterfaceReplyOp   = putInt32 2
  
-- | Binary instance for 'Version'.
instance Binary Version where
  get = do
    value <- getInt32
    return $
      case value of
        100 -> V100
        _   -> Version value
  
  put V100            = putInt32 100
  put (Version value) = put value
        
-- | Binary instance for 'Flags'.
instance Binary Flags where
  get = do
    value <- getInt32
    return $
      case value of
        0          -> BigEndian
        0x80000000 -> LittleEndian
        _          -> Flags value
        
  put BigEndian     = putInt32 0
  put LittleEndian  = putInt32 0x80000000
  put (Flags value) = put value

-- | Make an 'InterfaceRequest' message.
mkInterfaceRequest :: Version -> Flags -> Message
mkInterfaceRequest version flags =
  let payload = InterfaceRequest version flags
  in Message (header payload) payload

decodeProtocolPayload :: PayloadType -> LBS.ByteString -> ProtocolPayload
decodeProtocolPayload payloadType = runGet go
  where
    go :: Get ProtocolPayload
    go = 
      case payloadType of
        InterfaceRequestOp -> decodeInterfaceRequest    
        _                  -> error "Unsupported payload type"
        
decodeInterfaceRequest :: Get ProtocolPayload        
decodeInterfaceRequest = InterfaceRequest <$> get <*> get

getInt32 :: Get Int32
getInt32 = get
  
putInt32 :: Int32 -> Put
putInt32 = put