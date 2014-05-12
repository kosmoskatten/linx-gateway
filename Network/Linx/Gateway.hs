module Network.Linx.Gateway
       ( MessageCode (..)
       , Version (..)
       , Endianess (..)
       , encode
       , decode
       ) where

import Data.Binary
import Data.Int (Int32)
import Data.Word (Word32)

-- | Message codes describing the identities for requests and
-- replies. Not implementing codes marked as 'Not used' in the
-- protocol specification.
data MessageCode =
    InterfaceRequest
  | InterfaceReply
  | CreateRequest
  | CreateReply
  | DestroyRequest
  | DestroyReply
  | SendRequest
  | SendReply
  | ReceiveRequest
  | ReceiveReply
  | HuntRequest
  | HuntReply
  | AttachRequest
  | AttachReply
  | DetachRequest
  | DetachReply
  | NameRequest
  | NameReply
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
           
-- | Binary instance for 'MessageCode'.
instance Binary MessageCode where
  put InterfaceRequest  = putInt32 1
  put InterfaceReply    = putInt32 2
  put CreateRequest     = putInt32 7
  put CreateReply       = putInt32 8
  put DestroyRequest    = putInt32 9
  put DestroyReply      = putInt32 10
  put SendRequest       = putInt32 11
  put SendReply         = putInt32 12
  put ReceiveRequest    = putInt32 13
  put ReceiveReply      = putInt32 14
  put HuntRequest       = putInt32 15
  put HuntReply         = putInt32 16
  put AttachRequest     = putInt32 17
  put AttachReply       = putInt32 18
  put DetachRequest     = putInt32 19
  put DetachReply       = putInt32 20
  put NameRequest       = putInt32 21
  put NameReply         = putInt32 22
  get                   = do
    value <- get :: Get Int32
    case value of
      1  -> return InterfaceRequest
      2  -> return InterfaceReply
      7  -> return CreateRequest
      8  -> return CreateReply
      9  -> return DestroyRequest
      10 -> return DestroyReply
      11 -> return SendRequest
      12 -> return SendReply
      13 -> return ReceiveRequest
      14 -> return ReceiveReply
      15 -> return HuntRequest
      16 -> return HuntReply
      17 -> return AttachRequest
      18 -> return AttachReply
      19 -> return DetachRequest
      20 -> return DetachReply
      21 -> return NameRequest
      22 -> return NameReply
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

putInt32 :: Int32 -> Put
putInt32 = put

putWord32 :: Word32 -> Put
putWord32 = put