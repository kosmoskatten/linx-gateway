{-# LANGUAGE CPP #-}
module Main where

#include "SignalNumbers.h"

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import Network.Linx.Gateway
import Text.Printf
import PingMessage

-- | Simple LINX gateway program to implement a ping client sending
-- ping request messages to a ping server every second. When the ping
-- response is received the complete round trip time is calculated and
-- printed on stdout.
--
-- A LINX gatway server must be available for the program to work.
main :: IO ()
main = runPingClient =<< getArgs

runPingClient :: [String] -> IO ()
runPingClient [name, gateway, gatewayPort] = do
  -- Register itself in the gateway. Getting a Gateway instance in
  -- return.
  gw     <- create name gateway (Service gatewayPort)
  
  -- Ask the gateway server to tell its configured name.
  gwName <- askName gw
  printf "PingClient now connected to gateway '%s'. Pid: '%s'\n" 
         gwName (show $ self gw)
         
  -- Ask for a LINX connection to the ping server named "server". Once
  -- it's available enter the event loop.
  eventLoop gw =<< discoverAndSupervise gw "server"
runPingClient _ = 
  error "Run as PingClient <Client name> <Gateway address> <Gateway port>"
  
eventLoop :: Gateway -> Pid -> IO ()
eventLoop gw pid = do
  -- Wait for one second one either timeout, a ping response or an
  -- attach signal telling that we have lost contact to the ping
  -- server.
  maybeSig <- 
    receiveWithTimeout gw (Timeout 1000) $ Sel [PingResponseSig, AttachSig]
    
  -- Match what's happend.
  handleSignal maybeSig  
  where
    handleSignal :: Maybe (Pid, Signal) -> IO ()
    
    -- Timeout, send ping request.
    handleSignal Nothing = do
      ping <- encode <$> mkTimestampedPingRequest
      sendWithSelf gw pid $ Signal PingRequestSig ping
      eventLoop gw pid
      
    -- Lot contact to the ping server. Wait for the service to be
    -- available again.
    handleSignal (Just (lost, NumericSignal AttachSig)) = do
      printf "Lost contact to pid '%s'. Trying to reconnect ...\n" (show lost)
      eventLoop gw =<< discoverAndSupervise gw "server"
      
    -- Handle the ping response.
    handleSignal (Just (_, Signal PingResponseSig lbs)) = do
      rtt <- captureRTT $ decode lbs
      printf "Got PingResponse with RTT: '%s'\n" (show rtt)
      eventLoop gw pid
      
    -- Should not happen ...
    handleSignal (Just (from, something)) = do
      printf "Got unexpected '%s' from '%s'\n" (show from) (show something)
      eventLoop gw pid

discoverAndSupervise :: Gateway -> String -> IO Pid
discoverAndSupervise gw huntee = do
  -- Hunt for the ping server service ...
  _ <- hunt gw huntee $ NumericSignal HuntSig
  (pid, NumericSignal HuntSig) <- receive gw $ Sel [HuntSig]
  
  -- Once we have its pid do supervise it.
  attach gw pid $ NumericSignal AttachSig
  printf "PingClient now attached to server with pid: '%s'\n" (show pid)
  return pid
