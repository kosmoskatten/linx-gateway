{-# LANGUAGE DeriveGeneric #-}
module Network.Linx.Gateway
       ( MessageCode (..)
       , Version (..)
       , Endianess (..)
       , Status (..)
       , Message (..)
       , Payload
       , InterfaceRequest (..)
       , toMessage
       , encode
       , decode
       ) where

import Data.Binary
import Data.Int (Int32)
import Data.Word (Word32)
import GHC.Generics

-- | Message codes describing the identities for requests and
-- replies. Not implementing codes marked as 'Not used' in the
-- protocol specification.
data MessageCode =
    InterfaceRequestOp
  | InterfaceReplyOp
  | CreateRequestOp
  | CreateReplyOp
  | DestroyRequestOp
  | DestroyReplyOp
  | SendRequestOp
  | SendReplyOp
  | ReceiveRequestOp
  | ReceiveReplyOp
  | HuntRequestOp
  | HuntReplyOp
  | AttachRequestOp
  | AttachReplyOp
  | DetachRequestOp
  | DetachReplyOp
  | NameRequestOp
  | NameReplyOp
  deriving (Show, Eq)
           
-- | Protocol version. The currently only supported version is '100'.
data Version = 
  Version100           
  deriving (Show, Eq)
           
-- | Endianess.           
data Endianess =
    BigEndian
  | LittleEndian
  deriving (Show, Eq)
           
-- | Status of an request to the gateway server.
data Status =
    Error
  | Success
  deriving (Show, Eq)
           
data Message a = 
  Message { messageCode :: !MessageCode
          , payloadSize :: !Word32
          , payload     :: !a }
  deriving (Show, Eq)
           
class Payload a where
  code :: a -> MessageCode
  size :: a -> Word32

data InterfaceRequest =
  InterfaceRequest { version :: !Version
                   , flags   :: !Endianess }
  deriving (Show, Eq, Generic)  

toMessage :: (Binary a, Payload a) => a -> Message a
toMessage m = Message (code m) (size m) m

-- | Binary instance for 'MessageCode'.
instance Binary MessageCode where
  put InterfaceRequestOp  = putInt32 1
  put InterfaceReplyOp    = putInt32 2
  put CreateRequestOp     = putInt32 7
  put CreateReplyOp       = putInt32 8
  put DestroyRequestOp    = putInt32 9
  put DestroyReplyOp      = putInt32 10
  put SendRequestOp       = putInt32 11
  put SendReplyOp         = putInt32 12
  put ReceiveRequestOp    = putInt32 13
  put ReceiveReplyOp      = putInt32 14
  put HuntRequestOp       = putInt32 15
  put HuntReplyOp         = putInt32 16
  put AttachRequestOp     = putInt32 17
  put AttachReplyOp       = putInt32 18
  put DetachRequestOp     = putInt32 19
  put DetachReplyOp       = putInt32 20
  put NameRequestOp       = putInt32 21
  put NameReplyOp         = putInt32 22
  get                   = do
    value <- get :: Get Int32
    case value of
      1  -> return InterfaceRequestOp
      2  -> return InterfaceReplyOp
      7  -> return CreateRequestOp
      8  -> return CreateReplyOp
      9  -> return DestroyRequestOp
      10 -> return DestroyReplyOp
      11 -> return SendRequestOp
      12 -> return SendReplyOp
      13 -> return ReceiveRequestOp
      14 -> return ReceiveReplyOp
      15 -> return HuntRequestOp
      16 -> return HuntReplyOp
      17 -> return AttachRequestOp
      18 -> return AttachReplyOp
      19 -> return DetachRequestOp
      20 -> return DetachReplyOp
      21 -> return NameRequestOp
      22 -> return NameReplyOp
      _  -> error "Unexpected binary message code"
      
-- | Binary instance for 'Version'.
instance Binary Version where
  put Version100 = putInt32 100
  get            = do
    value <- get :: Get Int32
    case value of
      100 -> return Version100
      _   -> error "Unexpected binary version tag"

-- | Binary instance for 'Endianess'.
instance Binary Endianess where
  put BigEndian    = putWord32 0
  put LittleEndian = putWord32 0x80000000
  get              = do
    value <- get :: Get Word32
    case value of
      0          -> return BigEndian
      0x80000000 -> return LittleEndian
      _          -> error "Unexpected endianess"

-- | Binary instance for 'Status'.
instance Binary Status where
  put Error   = putInt32 (-1)
  put Success = putInt32 0
  get         = do
    value <- get :: Get Int32
    case value of
      (-1) -> return Error
      0    -> return Success
      _    -> error "Unexpected status value"

-- | Binary instance for 'InterfaceRequest'.
instance Binary InterfaceRequest

-- | Payload instance for 'InterfaceRequest'.
instance Payload InterfaceRequest where
  code _ = InterfaceRequestOp
  size _ = 8

putInt32 :: Int32 -> Put
putInt32 = put

putWord32 :: Word32 -> Put
putWord32 = put