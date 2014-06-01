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
       , mkHuntRequest
       , mkHuntReply
       , mkReceiveRequest
       , mkReceiveReply
       , mkSendRequest
       , mkSendReply
       , mkAttachRequest
       , mkAttachReply
       , mkDetachRequest
       , mkDetachReply
       , mkNameRequest
       , mkNameReply
       , headerSize
       , decodeHeader
       , decodeProtocolPayload
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get (runGet)
import Data.Int (Int32)
import GHC.Generics
import qualified Data.ByteString.Lazy as LBS

import Network.Linx.Gateway.BinaryInt32
  ( getInt32
  , putInt32
  )
import Network.Linx.Gateway.Types
  ( Status (..)
  , Length (..)
  , Index (..)
  , SigNo (..)
  , Version (..)
  , Flags (..)
  , CString (..)
  , User (..)
  , Pid (..)
  , Timeout (..)
  , Attref (..)
  , mkCString
  , cstrlen
  , toLength
  , asInt
  )
import Network.Linx.Gateway.Signal
  ( Signal (..)
  , payloadSize
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
    
  -- | This request has two puposes. The client sends this request to
  -- retrieve information about the gateway server, e.g. supported
  -- requests, protocol verions etc. It is also used as a
  -- "ping-message" to check that the server is alive [..]"
  | InterfaceRequest { version :: !Version 
                     , flags   :: !Flags }
  | InterfaceReply { status       :: !Status 
                   , version      :: !Version 
                   , flags        :: !Flags 
                   , typesLen     :: !Length 
                   , payloadTypes :: ![PayloadType] }
    
  -- | This request it used to create a "client" instance on the server
  -- that the client communicated with."
  | CreateRequest { user   :: !User
                  , myName :: !CString }
  | CreateReply { status     :: !Status
                , pid        :: !Pid
                , maxSigSize :: !Length }
    
  -- | This request is used to remove a "client" instance on the server,
  -- i.e. end the session that was started with the create request.
  | DestroyRequest { pid :: !Pid }
  | DestroyReply   {status :: !Status}
    
  -- | This request is to used to ask the gateway server to execute a
  -- hunt call.
  | HuntRequest { user      :: !User
                , nameIndex :: !Index
                , sigIndex  :: !Index
                , signal    :: !Signal
                , huntName  :: !CString }
  | HuntReply { status :: !Status
              , pid    :: !Pid }
    
  -- | This request is used to ask the server to execute a receive or
  -- receive_w_tmo call. It differs from other requests, because the
  -- client may send a second receive request or an interface request
  -- before it has received the reply from the previous receive
  -- request. The client may send a second receive request to cancel
  -- the first one. Beware that server may already have sent a receive
  -- reply before the "cancel request" was received, in this case the
  -- client must also wait for the "cancel reply". The client may send
  -- an interface request to the server, which returns an interface
  -- reply. This is used by the client to detect if the server has
  -- died while waiting for a receive reply.
  | ReceiveRequest { timeout    :: !Timeout
                   , sigselLen  :: !Length
                   , sigselList :: ![SigNo] }
  | ReceiveReply { status       :: !Status
                 , senderPid    :: !Pid
                 , addresseePid :: !Pid
                 , signal       :: !Signal }
    
  -- | This request is used to ask the gateway server to execute a
  -- send or send_w_s call.
  | SendRequest { fromPid :: !Pid
                , destPid :: !Pid
                , signal  :: !Signal }
  | SendReply { status :: !Status }
    
  -- | This request is used to ask the gateway server to execute an
  -- attach call.
  | AttachRequest { pid :: !Pid
                  , signal :: !Signal }
  | AttachReply { status :: !Status
                , attref :: !Attref }
  -- | This request is used to ask the gateway server to execute a
  -- detach call.
  | DetachRequest { attref :: !Attref }
  | DetachReply { status :: !Status }
    
  -- | This request is to retrieve the gateway server's mame.
  | NameRequest { reserved :: !Int32 }
  | NameReply { status  :: !Status
              , nameLen :: !Length
              , name    :: !CString }
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
    Header InterfaceReplyOp (Length $ 16 + 4 * (asInt $ typesLen msg))
  header msg@CreateRequest {}  = 
    let strlen = asInt $ cstrlen (myName msg)
    in Header CreateRequestOp $ Length $ 4 + strlen
  header CreateReply {}        = Header CreateReplyOp (Length 12)
  header DestroyRequest {}     = Header DestroyRequestOp (Length 4)
  header DestroyReply {}       = Header DestroyReplyOp (Length 4)
  header msg@HuntRequest {}    =
    let huntNameLen  = asInt $ cstrlen (huntName msg)
        payloadSize' = asInt $ payloadSize (signal msg)
    in Header HuntRequestOp (Length $ 12 + payloadSize' + huntNameLen)
  header HuntReply {}          = Header HuntReplyOp (Length 8)
  header msg@ReceiveRequest {} =
    let sigselLen' = asInt $ sigselLen msg
    in Header ReceiveRequestOp (Length $ 8 + 4 * sigselLen')
  header msg@ReceiveReply {}   =
    let payloadSize' = asInt $ payloadSize (signal msg)
    in Header ReceiveReplyOp (Length $ 12 + payloadSize')
  header msg@SendRequest {}    =
    let payloadSize' = asInt $ payloadSize (signal msg)
    in Header SendRequestOp (Length $ 8 + payloadSize')
  header SendReply {}          = Header SendReplyOp (Length 4)
  header msg@AttachRequest {}  =
    let payloadSize' = asInt $ payloadSize (signal msg)
    in Header AttachRequestOp (Length $ 4 + payloadSize')
  header AttachReply {}        = Header AttachReplyOp (Length 8)
  header DetachRequest {}      = Header DetachRequestOp (Length 4)
  header DetachReply {}        = Header DetachReplyOp (Length 4)
  header NameRequest {}        = Header NameRequestOp (Length 4)
  header msg@NameReply {}      =
    let nameLen' = asInt $ nameLen msg
    in Header NameReplyOp (Length $ 8 + nameLen')

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
  put msg@HuntRequest {}      =
    put (user msg) >> put (nameIndex msg) >> put (sigIndex msg)
                   >> put (signal msg) >> put (huntName msg)
  put msg@HuntReply {}        = put (status msg) >> put (pid msg)
  put msg@ReceiveRequest {}   = put (timeout msg) >> put (sigselLen msg)
                                                  >> putList (sigselList msg)
  put msg@ReceiveReply {}     = put (status msg) >> put (senderPid msg)
                                                 >> put (addresseePid msg)
                                                 >> put (signal msg)
  put msg@SendRequest {}      = put (fromPid msg) >> put (destPid msg)
                                                  >> put (signal msg)
  put msg@SendReply {}        = put (status msg)
  put msg@AttachRequest {}    = put (pid msg) >> put (signal msg)
  put msg@AttachReply {}      = put (status msg) >> put (attref msg)
  put msg@DetachRequest {}    = put (attref msg)
  put msg@DetachReply {}      = put (status msg)
  put msg@NameRequest {}      = put (reserved msg)
  put msg@NameReply {}        = put (status msg) >> put (nameLen msg)
                                                 >> put (name msg)

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
  let typesLength = toLength $ length types
      payload = InterfaceReply Success version' flags' typesLength types
  in Message (header payload) payload

-- | Make a 'CreateRequest' message.
mkCreateRequest :: String -> Message
mkCreateRequest name' =
  let cstring = mkCString name'
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

-- | Make a 'HuntRequest' message.
mkHuntRequest :: Signal -> CString -> Message
mkHuntRequest signal' huntName' =
  let nameIndex' = calcNameIndex signal'
      sigIndex'  = Index 0
      payload    = HuntRequest AlwaysZero nameIndex' sigIndex' signal' huntName'
  in Message (header payload) payload
  where
    calcNameIndex :: Signal -> Index
    calcNameIndex NoSignal         = Index 0
    calcNameIndex NumericSignal {} = Index 0
    calcNameIndex sig              = 
      let Length len = payloadSize sig
      in Index $ len - 8

-- | Make a 'HuntReply' message.
mkHuntReply :: Pid -> Message
mkHuntReply pid' =
  let payload = HuntReply Success pid'
  in Message (header payload) payload

-- | Make a 'ReceiveRequest' message.
mkReceiveRequest :: Timeout -> [SigNo] -> Message
mkReceiveRequest tmo sigNos =
  let sigselLen' = toLength $ length sigNos
      payload    = ReceiveRequest tmo sigselLen' sigNos
  in Message (header payload) payload

-- | Make a 'ReceiveReply' message.
mkReceiveReply :: Pid -> Pid -> Signal -> Message
mkReceiveReply senderPid' addresseePid' signal' =
  let payload = ReceiveReply Success senderPid' addresseePid' signal'
  in Message (header payload) payload

-- | Make a 'SendRequest' message.
mkSendRequest :: Pid -> Pid -> Signal -> Message
mkSendRequest fromPid' destPid' signal' =
  let payload = SendRequest fromPid' destPid' signal'
  in Message (header payload) payload
     
-- | Make 'SendReply' message.
mkSendReply :: Message
mkSendReply = 
  let payload = SendReply Success
  in Message (header payload) payload

-- | Make 'AttachRequest' message.
mkAttachRequest :: Pid -> Signal -> Message
mkAttachRequest pid' signal' =
  let payload = AttachRequest pid' signal'
  in Message (header payload) payload

-- | Make 'AttachReply' message.
mkAttachReply :: Attref -> Message
mkAttachReply attref' =
  let payload = AttachReply Success attref'
  in Message (header payload) payload

-- | Make 'DetachRequest' message.
mkDetachRequest :: Attref -> Message
mkDetachRequest attref' =
  let payload = DetachRequest attref'
  in Message (header payload) payload
     
-- | Make 'DetachReply' message.
mkDetachReply :: Message
mkDetachReply =
  let payload = DetachReply Success
  in Message (header payload) payload

-- | Make 'NameRequest' message.
mkNameRequest :: Message
mkNameRequest =
  let payload = NameRequest 0
  in Message (header payload) payload
     
-- | Make 'NameReply' message.
mkNameReply :: String -> Message
mkNameReply name' =
  let cstring = mkCString name'
      payload = NameReply Success (cstrlen cstring) cstring
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
        HuntRequestOp      -> decodeHuntRequest
        HuntReplyOp        -> decodeHuntReply
        ReceiveRequestOp   -> decodeReceiveRequest
        ReceiveReplyOp     -> decodeReceiveReply
        SendRequestOp      -> decodeSendRequest
        SendReplyOp        -> decodeSendReply
        AttachRequestOp    -> decodeAttachRequest
        AttachReplyOp      -> decodeAttachReply
        DetachRequestOp    -> decodeDetachRequest
        DetachReplyOp      -> decodeDetachReply
        NameRequestOp      -> decodeNameRequest
        NameReplyOp        -> decodeNameReply
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

-- The decoding of hunt requests is simplified to only accept a layout
-- where the hunt name is laid out after the signal data. This is not
-- any problem for this implementation as it not is implementing the
-- server role.
decodeHuntRequest :: Get ProtocolPayload
decodeHuntRequest = HuntRequest <$> get <*> get <*> get <*> get <*> get

decodeHuntReply :: Get ProtocolPayload
decodeHuntReply = HuntReply <$> get <*> get

decodeReceiveRequest :: Get ProtocolPayload
decodeReceiveRequest = do
  timeout'    <- get
  sigselLen'  <- get
  sigselList' <- getList sigselLen'
  return $ ReceiveRequest timeout' sigselLen' sigselList'  

decodeReceiveReply :: Get ProtocolPayload
decodeReceiveReply = ReceiveReply <$> get <*> get <*> get <*> get

decodeSendRequest :: Get ProtocolPayload
decodeSendRequest = SendRequest <$> get <*> get <*> get

decodeSendReply :: Get ProtocolPayload
decodeSendReply = SendReply <$> get

decodeAttachRequest :: Get ProtocolPayload
decodeAttachRequest = AttachRequest <$> get <*> get

decodeAttachReply :: Get ProtocolPayload
decodeAttachReply = AttachReply <$> get <*> get

decodeDetachRequest :: Get ProtocolPayload
decodeDetachRequest = DetachRequest <$> get

decodeDetachReply :: Get ProtocolPayload
decodeDetachReply = DetachReply <$> get

decodeNameRequest :: Get ProtocolPayload
decodeNameRequest = NameRequest <$> get

decodeNameReply :: Get ProtocolPayload
decodeNameReply = NameReply <$> get <*> get <*> get

putList :: Binary a => [a] -> Put
putList = mapM_ put

getList :: Binary a => Length -> Get [a]
getList len = replicateM (asInt len) get