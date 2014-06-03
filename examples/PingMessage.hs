{-# LANGUAGE DeriveGeneric #-}
module PingMessage
       ( PingRequest
       , PingResponse
       , mkTimestampedPingRequest
       , mkTimestampedPingResponse
       , captureRTT
       , pingRequestSig
       , pingResponseSig
       , encode
       , decode
       ) where

import Control.Applicative ((<$>))
import Data.Binary
import Data.Time (getCurrentTime)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Network.Linx.Gateway (SigNo (..))
import GHC.Generics

newtype PingRequest = PingRequest String
  deriving (Show, Generic)
           
newtype PingResponse = PingResponse String
  deriving (Show, Generic)
           
instance Binary PingRequest
instance Binary PingResponse

mkTimestampedPingRequest :: IO PingRequest
mkTimestampedPingRequest = PingRequest . show <$> getCurrentTime

mkTimestampedPingResponse :: PingRequest -> PingResponse
mkTimestampedPingResponse (PingRequest timestamp) = PingResponse timestamp

captureRTT :: PingResponse -> IO NominalDiffTime
captureRTT (PingResponse timestamp) = do
  currentTime <- getCurrentTime
  let startTime = read timestamp
  return $ diffUTCTime currentTime startTime
  
pingRequestSig :: SigNo
pingRequestSig = SigNo 100

pingResponseSig :: SigNo
pingResponseSig = SigNo 101
  