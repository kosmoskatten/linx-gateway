module Network.Linx.Gateway
       ( MessageCode (..)
       , Version (..)
       , Endianess (..)
       , Status (..)
       , User (..)
       , Message (..)
       , Payload
       , ProtocolPayload (..)
       , toMessage
       , mkSendRequest
       , mkSendReply
       , encode
       , decode
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Put (putLazyByteString)
import Data.Binary.Get (getLazyByteString, getLazyByteStringNul)
import Data.Int (Int32)
import qualified Data.ByteString.Lazy.Char8 as LBS

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
           
-- | A user id when creating an instance on the gateway server.           
data User =
  AlwaysZero
  deriving (Show, Eq)
           
-- | A serializable Linx message.           
data Message = 
  Message !MessageCode !Int32 !ProtocolPayload
  deriving (Show, Eq)
           
-- | Payload typeclass to pick message code and byte length for a
-- serialized protocol payload.
class Payload a where
  messageCode :: a -> MessageCode
  payloadSize :: a -> Int32

data ProtocolPayload =
    InterfaceRequest !Version !Endianess
  | InterfaceReply !Status !Version !Endianess !Int32 ![MessageCode]
  | CreateRequest !User !LBS.ByteString
  | CreateReply !Status !Int32 !Int32
  | DestroyRequest !Int32
  | DestroyReply !Status
  | SendRequest !Int32 !Int32 !Int32 !Int32 !LBS.ByteString
  | SendReply !Status
  deriving (Show, Eq)  

-- | Convert a Linx protocol payload message to a serializable
-- message.
toMessage :: ProtocolPayload -> Message
toMessage m = Message (messageCode m) (payloadSize m) m

-- | Create a SendRequest protocol payload.
mkSendRequest :: Int32 -> Int32 -> Int32 -> LBS.ByteString -> ProtocolPayload
mkSendRequest fromPid destPid sigNo sigData =
  let sigLen = 4 + (fromIntegral $ LBS.length sigData)
  in SendRequest fromPid destPid sigLen sigNo sigData
     
-- | Create a SendReply protocol payload
mkSendReply :: Status -> ProtocolPayload
mkSendReply = SendReply

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

-- | Binary instance for 'User'.
instance Binary User where
  put AlwaysZero = putInt32 0
  get            = do
    value <- get :: Get Int32
    case value of
      0 -> return AlwaysZero
      _ -> error "Unexpected user value"

-- | Binary instance for 'Message'.
instance Binary Message where
  put (Message code size payload) = do
    put code
    put size
    case payload of
      InterfaceRequest version flags -> put version >> put flags        
      InterfaceReply status version flags len codes ->
        put status >> put version >> put flags >> put len >> putList codes
      CreateRequest user name -> put user >> putLazyByteStringNul name        
      CreateReply status pid sigSize -> put status >> put pid >> put sigSize
      DestroyRequest pid -> put pid
      DestroyReply status -> put status
      SendRequest fromPid destPid len sigNo sigData ->
        put fromPid >> put destPid >> put len 
                    >> put sigNo >> putLazyByteString sigData
      SendReply status -> put status
  
  get                             = do
    code <- get
    size <- get
    payload <- 
      case code of
        InterfaceRequestOp -> InterfaceRequest <$> get <*> get
        
        InterfaceReplyOp -> do
          status <- get
          version <- get
          flags <- get
          len <- get
          codes <- getList $ fromIntegral len
          return $ InterfaceReply status version flags len codes
          
        CreateRequestOp -> CreateRequest <$> get <*> getLazyByteStringNul
        CreateReplyOp -> CreateReply <$> get <*> get <*> get
        DestroyRequestOp -> DestroyRequest <$> get
        DestroyReplyOp -> DestroyReply <$> get
        
        SendRequestOp -> do
          fromPid <- get
          destPid <- get
          sigLen <- get :: Get Int32
          sigNo <- get
          sigData <- getLazyByteString (fromIntegral $ sigLen - 4)
          return $ mkSendRequest fromPid destPid sigNo sigData
          
        SendReplyOp -> mkSendReply <$> get
          
    return $ Message code size payload

-- | Payload instance for 'ProtocolPayload'.
instance Payload ProtocolPayload where
  messageCode (InterfaceRequest _ _)     = InterfaceRequestOp
  messageCode (InterfaceReply _ _ _ _ _) = InterfaceReplyOp
  messageCode (CreateRequest _ _)        = CreateRequestOp
  messageCode (CreateReply _ _ _)        = CreateReplyOp
  messageCode (DestroyRequest _)         = DestroyRequestOp
  messageCode (DestroyReply _)           = DestroyReplyOp
  messageCode (SendRequest _ _ _ _ _)    = SendRequestOp
  messageCode (SendReply _)              = SendReplyOp
  
  payloadSize (InterfaceRequest _ _)       = 8
  payloadSize (InterfaceReply _ _ _ len _) = 16 + (len * 4)
  payloadSize (CreateRequest _ name) = 4 + (fromIntegral $ LBS.length name) + 1
  payloadSize (CreateReply _ _ _) = 12
  payloadSize (DestroyRequest _) = 4
  payloadSize (DestroyReply _) = 4
  payloadSize (SendRequest _ _ len _ _) = 12 + len
  payloadSize (SendReply _) = 4

putInt32 :: Int32 -> Put
putInt32 = put

putWord32 :: Word32 -> Put
putWord32 = put

putLazyByteStringNul :: LBS.ByteString -> Put
putLazyByteStringNul lbs = do
  putLazyByteString lbs
  putWord8 0

putList :: Binary a => [a] -> Put
putList = mapM_ put

getList :: Binary a => Int -> Get [a]
getList len = replicateM len get
