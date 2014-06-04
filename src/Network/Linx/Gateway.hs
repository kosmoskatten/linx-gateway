-- | Implementation of the Enea LINX Gateway protocol in Haskell. More
-- information about LINX and the LINX Gateway can be found on its
-- project page on Sourceforge: <http://sourceforge.net/projects/linx/>
--
-- [LINX Gateway Documentation]
--
-- User's guide: <http://linx.sourceforge.net/linxdoc/doc/usersguide/UsersGuide_LINX_Gateway.html>
--
-- LINX protcols: <http://linx.sourceforge.net/linxdoc/doc/linxprotocols/book-linx-protocols-html/index.html>
--
-- [Example application]
--
-- Bundled with this software package is an example application
-- consisting of one ping server program and one ping client
-- program. The example programs are demonstrating several aspects of
-- the gateway API.
--
-- The code can be browsed in the examples directory at the project's
-- GitHub: <https://github.com/kosmoskatten/linx-gateway>
-- 
-- In order to run the examples a LINX Gateserver must be setup and be
-- available in your IP network. For the samples below the gateway
-- server is running at 192.168.122.8 port 21768.
--
-- > cabal configure
-- > cabal build
-- > cabal run PingClient client 192.168.122.8 21768
-- > cabal run PingServer 192.168.122.8 21768
--
-- The order in which the server and the client is started is not
-- important. The client is also supervising the server, so if the
-- server is terminated the client is trying to reconnect to the
-- server again once it's restarted.
--
-- Several clients can be started.
module Network.Linx.Gateway
       ( Gateway (..)
       , HostName
       , PortID (..)
       , Signal (..)
       , SigNo (..)
       , SignalSelector (..)
       , Pid
       , Timeout (..)
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
import Data.Maybe (fromJust)
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
import Network.Linx.Gateway.Signal 
  ( Signal (..)
  , SignalSelector (..)
  )
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
  
-- | Gateway instance.
data Gateway =
  Gateway { 
      -- | The socket handle towards the gateway server.
      handle    :: !Handle
      -- | The LINX 'Pid' of the gateway instance.
    , self      :: !Pid
      -- | The max length of a 'Signal' payload the gateway
      -- server is accepting.
    , maxSignal :: !Length
      -- | The type of operations in the gateway protocol that
      -- the gateway server is accepting.
    , accept    :: ![PayloadType]}
  deriving (Show, Eq)
           
-- | Create a new client instance in the gateway. The gateway is
-- addressed by a hostname and a port id.
create :: String -> HostName -> PortID -> IO Gateway
create name' hostname port = do
  gw          <- connectTo hostname port
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

-- | Ask the gateway server to execute a receive call with the
-- specified timeout value. If no signal is received within the time
-- the value of 'Nothing' is returned.
receiveWithTimeout :: Gateway -> Timeout -> SignalSelector
                   -> IO (Maybe (Pid, Signal))
receiveWithTimeout gw tmo sigSel' = do
  reply <- expectPayload (handle gw) 
    =<< talkGateway (handle gw) (mkReceiveRequest tmo sigSel')
  return $
    case reply of
      ReceiveReply Success (Pid 0) (Pid 0) NoSignal -> Nothing
      ReceiveReply Success senderPid' _ signal'     -> 
        Just (senderPid', signal')
      _                                             -> Nothing
      
-- | Ask the gateway server to execute a receive call with infinitely
-- long waiting time.
receive :: Gateway -> SignalSelector -> IO (Pid, Signal)
receive gw sigSel' = fromJust <$> receiveWithTimeout gw Infinity sigSel'
  
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