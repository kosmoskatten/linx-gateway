{-# LANGUAGE DeriveGeneric #-}
module Network.Linx.Gateway.Message 
       ( Message (..)
       , Header (..)
       , ProtocolPayload (..)
       , PayloadType (..)
       , encode
       , mkInterfaceRequest
       , mkInterfaceReply
       , mkCreateRequest
       , mkCreateReply
       , mkDestroyRequest
       , mkDestroyReply
       , headerSize
       , decodeHeader
       , decodeProtocolPayload
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get (runGet)
import GHC.Generics
import qualified Data.ByteString.Lazy as LBS

import Network.Linx.Gateway.BinaryInt32
  ( getInt32
  , putInt32
  )
import Network.Linx.Gateway.Types
  ( Status (..)
  , Length (..)
  , Version (..)
  , Flags (..)
  , CString (..)
  , User (..)
  , Pid (..)
  , mkCString
  )

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
  | CreateRequest { user   :: !User
                  , myName :: !CString }
  | CreateReply { status     :: !Status
                , pid        :: !Pid
                , maxSigSize :: !Length }
    
  -- This record is used to remove a "client" instance on the server,
  -- i.e. end the session that was started with the create request.
  | DestroyRequest { pid :: !Pid }
  | DestroyReply   {status :: !Status}
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
           
-- | Payload class. To be implemented by ProtocolPayload.
class Payload a where
  header :: a -> Header

-- | Generic binary instances.
instance Binary Header

instance Payload ProtocolPayload where
  header FailedRequest         = error "Shall not be called this way"
  header InterfaceRequest {}   = Header InterfaceRequestOp (Length 8)
  header msg@InterfaceReply {} = 
    let (Length len) = typesLen msg
    in Header InterfaceReplyOp (Length $ 16 + (4 * len))
  header msg@CreateRequest {}  = 
    let (CString lbs) = myName msg
        len           = Length $ 4 + (fromIntegral $ LBS.length lbs) + 1
    in Header CreateRequestOp len
  header CreateReply {}        = Header CreateReplyOp (Length 12)
  header DestroyRequest {}     = Header DestroyRequestOp (Length 4)
  header DestroyReply {}       = Header DestroyReplyOp (Length 4)

-- | Binary instance for 'Message'.
instance Binary Message where
  get = error "Shall not be called this way"
  put (Message msgHeader msgPayload) = put msgHeader >> put msgPayload

-- | Binary instance for 'ProtocolPayload'.
instance Binary ProtocolPayload where
  get = error "Shall not be called this way"
  put FailedRequest = error "Shall not be called this way"
  put msg@InterfaceRequest {} = put (version msg) >> put (flags msg)
  put msg@InterfaceReply {}   = 
    put (status msg) >> put (version msg) >> put (flags msg) 
                     >> put (typesLen msg) >> putList (payloadTypes msg)
  put msg@CreateRequest {}    = put (user msg) >> put (myName msg)
  put msg@CreateReply {}      = 
    put (status msg) >> put (pid msg) >> put (maxSigSize msg)
  put msg@DestroyRequest {}   = put (pid msg)
  put msg@DestroyReply {}     = put (status msg)

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

-- | Make a 'CreateRequest' message.
mkCreateRequest :: String -> Message
mkCreateRequest name =
  let cstring = mkCString name
      payload = CreateRequest AlwaysZero cstring
  in Message (header payload) payload

-- | Make a 'CreateReply' message.
mkCreateReply :: Pid -> Length -> Message
mkCreateReply pid' maxSigSize' =
  let payload = CreateReply Success pid' maxSigSize'
  in Message (header payload) payload

-- | Make a 'DestroyRequest' message.
mkDestroyRequest :: Pid -> Message
mkDestroyRequest pid' =
  let payload = DestroyRequest pid'
  in Message (header payload) payload
     
-- | Make a 'DestroyReply' message.
mkDestroyReply :: Message
mkDestroyReply =
  let payload = DestroyReply Success
  in Message (header payload) payload

-- | Get the header size in bytes.
headerSize :: Length
headerSize = Length 8

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
        CreateRequestOp    -> decodeCreateRequest
        CreateReplyOp      -> decodeCreateReply
        DestroyRequestOp   -> decodeDestroyRequest
        DestroyReplyOp     -> decodeDestroyReply
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
  
decodeCreateRequest :: Get ProtocolPayload
decodeCreateRequest = CreateRequest <$> get <*> get

decodeCreateReply :: Get ProtocolPayload
decodeCreateReply = CreateReply <$> get <*> get <*> get

decodeDestroyRequest :: Get ProtocolPayload
decodeDestroyRequest = DestroyRequest <$> get

decodeDestroyReply :: Get ProtocolPayload
decodeDestroyReply = DestroyReply <$> get

putList :: Binary a => [a] -> Put
putList = mapM_ put

getList :: Binary a => Length -> Get [a]
getList (Length len) = replicateM (fromIntegral len) get