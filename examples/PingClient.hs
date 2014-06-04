{-# LANGUAGE CPP #-}
module Main where

#include "SignalNumbers.h"

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import Network.Linx.Gateway
import Text.Printf
import PingMessage

main :: IO ()
main = runPingClient =<< getArgs

runPingClient :: [String] -> IO ()
runPingClient [name, gateway, gatewayPort] = do
  gw     <- create name gateway (Service gatewayPort)
  gwName <- askName gw
  printf "PingClient now connected to gateway '%s'. Pid: '%s'\n" 
         gwName (show $ self gw)
  eventLoop gw =<< discoverAndSupervise gw "server"
runPingClient _ = 
  error "Run as PingClient <Client name> <Gateway address> <Gateway port>"
  
eventLoop :: Gateway -> Pid -> IO ()
eventLoop gw pid = do
  -- Wait for one second ...
  maybeSig <- 
    receiveWithTimeout gw (Timeout 1000) $ Sel [PingResponseSig, AttachSig]
  handleSignal maybeSig  
  where
    handleSignal :: Maybe (Pid, Signal) -> IO ()
    handleSignal Nothing = do
      ping <- encode <$> mkTimestampedPingRequest
      sendWithSelf gw pid $ Signal PingRequestSig ping
      eventLoop gw pid
      
    handleSignal (Just (lost, NumericSignal AttachSig)) = do
      printf "Lost contact to pid '%s'. Trying to reconnect ...\n" (show lost)
      eventLoop gw =<< discoverAndSupervise gw "server"
      
    handleSignal (Just (_, Signal PingResponseSig lbs)) = do
      rtt <- captureRTT $ decode lbs
      printf "Got PingResponse with RTT: '%s'\n" (show rtt)
      eventLoop gw pid
      
    handleSignal (Just (from, something)) = do
      printf "Got unexpected '%s' from '%s'\n" (show from) (show something)
      eventLoop gw pid

discoverAndSupervise :: Gateway -> String -> IO Pid
discoverAndSupervise gw huntee = do
  _ <- hunt gw huntee $ NumericSignal HuntSig
  (pid, NumericSignal HuntSig) <- receive gw $ Sel [HuntSig]
  attach gw pid $ NumericSignal AttachSig
  printf "PingClient now attached to server with pid: '%s'\n" (show pid)
  return pid
