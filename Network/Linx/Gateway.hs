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
  , encode
  , mkInterfaceRequest
  , headerSize  
  , decodeHeader
  )
import System.IO (Handle)
  
-- | Record describing a gateway connection.
data Gateway =
  Gateway { handle :: !Handle }
  deriving (Show, Eq)
           
-- | Create a new client instance in the gateway.
create :: String -> HostName -> PortID -> IO Gateway
create name hostname port = do
  hGw <- connectTo hostname port
  LBS.hPut hGw (encode $ mkInterfaceRequest V100 BigEndian)
  ifcReplyHeader <- decodeHeader <$> LBS.hGet hGw headerSize
  print ifcReplyHeader
  return $ Gateway hGw