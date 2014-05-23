module Network.Linx.Gateway
       ( Gateway (..)
       , HostName
       , PortID (..)
       , create
       ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as LBS
import Network (HostName, PortID (..), connectTo)
import Network.Linx.Gateway.Message
  ( Message (..)
  , Header (..)
  , Version (..)
  , Flags (..)
  , Length (..)
  , Pid
  , ProtocolPayload (..)
  , PayloadType (..)
  , encode
  , mkInterfaceRequest
  , mkCreateRequest
  , headerSize  
  , decodeHeader
  , decodeProtocolPayload
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
  hGw <- connectTo hostname port
  ifcReplyHeader <- talkGateway hGw $ mkInterfaceRequest V100 BigEndian
  ifcReplyPayload <- expectPayload hGw ifcReplyHeader
  
  print ifcReplyHeader
  print ifcReplyPayload
  
  crReplyHeader <- talkGateway hGw $ mkCreateRequest name
  crReplyPayload <- expectPayload hGw crReplyHeader
  
  print crReplyHeader
  print crReplyPayload
  return $ Gateway hGw (pid crReplyPayload)
                       (maxSigSize crReplyPayload)
                       (payloadTypes ifcReplyPayload)

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