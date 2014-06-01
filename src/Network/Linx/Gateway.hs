module Network.Linx.Gateway
       ( Gateway (..)
       , HostName
       , PortID (..)
       , Signal (..)
       , create
       , destroy
       , hunt
       , receiveWithTimeout
       , receive
       , sendWithSender
       , sendWithSelf
       , attach
       , detach
       , askName
       ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Network (HostName, PortID (..), connectTo)
import Network.Linx.Gateway.Message
  ( Message (..)
  , Header (..)
  , ProtocolPayload (..)
  , PayloadType (..)
  , encode
  , mkInterfaceRequest
  , mkCreateRequest
  , mkDestroyRequest
  , mkHuntRequest
  , mkReceiveRequest
  , mkSendRequest
  , mkAttachRequest
  , mkDetachRequest
  , mkNameRequest
  , headerSize  
  , decodeHeader
  , decodeProtocolPayload
  )
import Network.Linx.Gateway.Signal (Signal (..))
import Network.Linx.Gateway.Types
  ( Version (..)
  , Status (..)
  , Flags (..)
  , Length (..)
  , Pid (..)
  , Timeout (..)
  , SigNo (..)
  , Attref (..)
  , CString (..)
  , mkCString
  )
import System.IO (Handle)
  
-- | Record describing a gateway connection.
data Gateway =
  Gateway { handle    :: !Handle
          , self      :: !Pid
          , maxSignal :: !Length
          , accept    :: ![PayloadType]}
  deriving (Show, Eq)
           
-- | Create a new client instance in the gateway.
create :: String -> HostName -> PortID -> IO Gateway
create name' hostname port = do
  gw <- connectTo hostname port
  createReply <- expectPayload gw =<< talkGateway gw (mkCreateRequest name')
  ifReply     <- expectPayload gw 
                   =<< talkGateway gw  (mkInterfaceRequest V100 BigEndian)
  return $ Gateway gw (pid createReply)
                      (maxSigSize createReply)
                      (payloadTypes ifReply)
  
-- | Destroy a client.
destroy :: Gateway -> IO ()
destroy gw = do
  _ <- expectPayload (handle gw) 
         =<< talkGateway (handle gw) (mkDestroyRequest (self gw))
  return ()

-- | Ask the gateway server to execute a hunt call. If the hunted
-- process is available at the moment of the hunt its pid is returned
-- immediately.
hunt :: Gateway -> String -> Signal -> IO (Maybe Pid)
hunt gw client signal' = do
  reply <- expectPayload (handle gw)
    =<< talkGateway (handle gw) (mkHuntRequest signal' (mkCString client))
  let pid' = pid reply
  return $
    case pid' of
      Pid 0 -> Nothing
      _     -> Just pid'

-- | Ask the gateway server to execute a hunt call.
receiveWithTimeout :: Gateway -> Timeout -> [SigNo] 
                   -> IO (Maybe (Pid, Signal))
receiveWithTimeout gw tmo sigNos = do
  reply <- expectPayload (handle gw) 
    =<< talkGateway (handle gw) (mkReceiveRequest tmo sigNos)
  return $
    case reply of
      ReceiveReply Success (Pid 0) (Pid 0) NoSignal -> Nothing
      ReceiveReply Success senderPid' _ signal'     -> 
        Just (senderPid', signal')
      _                                             -> Nothing
      
receive :: Gateway -> [SigNo] -> IO (Maybe (Pid, Signal))
receive gw = receiveWithTimeout gw Infinity
  
-- | Ask the gateway server to execute a send_w_s call.
sendWithSender :: Gateway -> Pid -> Pid -> Signal -> IO ()
sendWithSender gw fromPid' destPid' signal' = do
  _ <- expectPayload (handle gw)
    =<< talkGateway (handle gw) (mkSendRequest fromPid' destPid' signal')
  return ()
  
-- | Ask the gateway server to execute a send call_w_s call where the
-- sender is the pid stored in the gateway record.
sendWithSelf :: Gateway -> Pid -> Signal -> IO ()
sendWithSelf gw = sendWithSender gw (self gw)
             
-- | Ask the gateway server to execute an attach call.
attach :: Gateway -> Pid -> Signal -> IO Attref
attach gw pid' signal' = do
  reply <- expectPayload (handle gw)
    =<< talkGateway (handle gw) (mkAttachRequest pid' signal')
  return $ attref reply
  
-- | Ask the gateway server to execute a detach call.
detach :: Gateway -> Attref -> IO ()
detach gw attref' = do
  _ <- expectPayload (handle gw) 
    =<< talkGateway (handle gw) (mkDetachRequest attref')
  return ()
    
-- | Ask the gateway server about its name.
askName :: Gateway -> IO String
askName gw = do
  reply <- expectPayload (handle gw)
    =<< talkGateway (handle gw) mkNameRequest
  let CString lbs = name reply
  return $ LBSC.unpack lbs

talkGateway :: Handle -> Message -> IO Header
talkGateway hGw message = do
  LBS.hPut hGw $ encode message
  decodeHeader <$> readGateway hGw headerSize
  
expectPayload :: Handle -> Header -> IO ProtocolPayload
expectPayload hGw header =
  decodeProtocolPayload (payloadType header) 
    <$> readGateway hGw (payloadLength header)
  
readGateway :: Handle -> Length -> IO LBS.ByteString
readGateway hGw (Length len) = LBS.hGet hGw (fromIntegral len)