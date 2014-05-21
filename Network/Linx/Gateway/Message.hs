{-# LANGUAGE DeriveGeneric #-}
module Network.Linx.Gateway.Message 
       ( Message (..)
       , Header (..)
       , ProtocolPayload (..)
       , PayloadType (..)
       , Length (..)
       , Version (..)
       , Flags (..)
       , encode
       , mkInterfaceRequest
       , mkInterfaceReply
       , headerSize
       , decodeHeader
       , decodeProtocolPayload
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
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
  Header { payloadType   :: !PayloadType 
         , payloadLength :: !Length }
  deriving (Show, Eq, Generic)

-- | Protocol payload.
data ProtocolPayload =
    FailedRequest
  | InterfaceRequest { version :: !Version 
                     , flags   :: !Flags }
  | InterfaceReply { status       :: !Status 
                   , version      :: !Version 
                   , flags        :: !Flags 
                   , typesLen     :: !Length 
                   , payloadTypes :: ![PayloadType] }
  deriving (Show, Eq)

-- | Payload type discriminator.
data PayloadType =
    InterfaceRequestOp
  | InterfaceReplyOp
  | LoginRequestOp
  | ChallengeResponseOp    
  | ChallengeReplyOp
  | LoginReplyOp
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
           
-- | Status indicator           
data Status =
    Success
  | Error
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
  header FailedRequest         = error "Shall not be called this way"
  header InterfaceRequest {}   = Header InterfaceRequestOp (Length 8)
  header msg@InterfaceReply {} = 
    let (Length len) = typesLen msg
    in Header InterfaceReplyOp (Length $ 16 + (4 * len))

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
  put msg@InterfaceRequest {} = put (version msg) >> put (flags msg)
  put msg@InterfaceReply {} = 
    put (status msg) >> put (version msg) >> put (flags msg) 
                     >> put (typesLen msg) >> putList (payloadTypes msg)

-- | Binary instance for 'PayloadType'.
instance Binary PayloadType where
  get = do
    value <- getInt32
    return $
      case value of
        1  -> InterfaceRequestOp
        2  -> InterfaceReplyOp
        3  -> LoginRequestOp
        4  -> ChallengeResponseOp
        5  -> ChallengeReplyOp
        6  -> LoginReplyOp
        7  -> CreateRequestOp
        8  -> CreateReplyOp
        9  -> DestroyRequestOp
        10 -> DestroyReplyOp
        11 -> SendRequestOp
        12 -> SendReplyOp
        13 -> ReceiveRequestOp
        14 -> ReceiveReplyOp
        15 -> HuntRequestOp
        16 -> HuntReplyOp
        17 -> AttachRequestOp
        18 -> AttachReplyOp
        19 -> DetachRequestOp
        20 -> DetachReplyOp
        21 -> NameRequestOp
        22 -> NameReplyOp
        _ -> error $ "Unexpected discriminator code: " ++ show value
  
  put InterfaceRequestOp  = putInt32 1
  put InterfaceReplyOp    = putInt32 2
  put LoginRequestOp      = putInt32 3
  put ChallengeResponseOp = putInt32 4
  put ChallengeReplyOp    = putInt32 5
  put LoginReplyOp        = putInt32 6
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

-- | Binary instance for 'Status'.
instance Binary Status where
  get = do
    value <- getInt32
    return $
      case value of
        0 -> Success
        _ -> Error
        
  put Success = putInt32 0
  put Error   = putInt32 (-1)

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
        0 -> BigEndian
        1 -> LittleEndian
        _ -> Flags value
        
  put BigEndian     = putInt32 0
  put LittleEndian  = putInt32 1
  put (Flags value) = put value

-- | Make an 'InterfaceRequest' message.
mkInterfaceRequest :: Version -> Flags -> Message
mkInterfaceRequest version' flags' =
  let payload = InterfaceRequest version' flags'
  in Message (header payload) payload
     
-- | Make an 'InterfaceReply' message.
mkInterfaceReply :: Version -> Flags -> [PayloadType] -> Message
mkInterfaceReply version' flags' types =
  let typesLength = Length $ (fromIntegral . length) types
      payload = InterfaceReply Success version' flags' typesLength types
  in Message (header payload) payload

-- | Get the header size in bytes.
headerSize :: Int
headerSize = 8

-- | Decode the header.
decodeHeader :: LBS.ByteString -> Header
decodeHeader = decode

-- | Decode the protocol payload.
decodeProtocolPayload :: PayloadType -> LBS.ByteString -> ProtocolPayload
decodeProtocolPayload payloadType' = runGet go
  where
    go :: Get ProtocolPayload
    go = 
      case payloadType' of
        InterfaceRequestOp -> decodeInterfaceRequest
        InterfaceReplyOp   -> decodeInterfaceReply
        _                  -> error "Unsupported payload type"
        
decodeInterfaceRequest :: Get ProtocolPayload        
decodeInterfaceRequest = InterfaceRequest <$> get <*> get

decodeInterfaceReply :: Get ProtocolPayload
decodeInterfaceReply = do
  status'       <- get
  version'      <- get
  flags'        <- get
  typesLen'     <- get
  payloadTypes' <- getList typesLen'
  return $ InterfaceReply status' version' flags' typesLen' payloadTypes'

getInt32 :: Get Int32
getInt32 = get
  
putInt32 :: Int32 -> Put
putInt32 = put

putList :: Binary a => [a] -> Put
putList = mapM_ put

getList :: Binary a => Length -> Get [a]
getList (Length len) = replicateM (fromIntegral len) get