module Network.Linx.Gateway
       ( Gateway (..)
       , HostName
       , PortID (..)
       , Signal (..)
       , create
       , destroy
       , hunt
       ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as LBS
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
  , headerSize  
  , decodeHeader
  , decodeProtocolPayload
  )
import Network.Linx.Gateway.Signal (Signal (..))
import Network.Linx.Gateway.Types
  ( Version (..)
  , Flags (..)
  , Length (..)
  , Pid (..)
  , mkCString
  )
import System.IO (Handle)
  
-- | Record describing a gateway connection.
data Gateway =
  Gateway { handle    :: !Handle
          , process   :: !Pid
          , maxSignal :: !Length
          , accept    :: ![PayloadType]}
  deriving (Show, Eq)
           
-- | Create a new client instance in the gateway.
create :: String -> HostName -> PortID -> IO Gateway
create name hostname port = do
  gw <- connectTo hostname port
  createReply <- expectPayload gw =<< (talkGateway gw $ mkCreateRequest name)
  ifReply     <- expectPayload gw =<< (talkGateway gw $ 
                   mkInterfaceRequest V100 BigEndian)
  return $ Gateway gw (pid createReply)
                      (maxSigSize createReply)
                      (payloadTypes ifReply)
  
-- | Destroy a client.
destroy :: Gateway -> IO ()
destroy gw = do
  _ <- expectPayload (handle gw) 
         =<< (talkGateway (handle gw) $ mkDestroyRequest (process gw))
  return ()

-- Ask the gateway server to execute a hunt call. If the hunted
-- process is available at the moment of the hunt its pid is returned
-- immediately.
hunt :: Gateway -> String -> Signal -> IO (Maybe Pid)
hunt gw client signal' = do
  reply <- expectPayload (handle gw)
             =<< (talkGateway (handle gw) 
                   $ mkHuntRequest signal' (mkCString client))
  let pid' = pid reply
  return $
    case pid' of
      Pid 0 -> Nothing
      _     -> Just pid'

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