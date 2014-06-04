{-# LANGUAGE CPP #-}
module Main where

#include "SignalNumbers.h"

import Control.Monad (forever)
import System.Environment (getArgs)
import Network.Linx.Gateway 
  ( PortID (Service)
  , SigNo (..)
  , Signal (..)
  , SignalSelector (..)
  , create
  , askName
  , sendWithSelf
  , receive
  )
import Text.Printf
import PingMessage

main :: IO ()
main = runPingServer =<< getArgs

runPingServer :: [String] -> IO ()
runPingServer [gateway, port] = do
  gw <- create "server" gateway (Service port)
  gwName <- askName gw
  printf "PingServer now connectected to gateway '%s'\n" gwName
  forever $ do
    (from, Signal PingRequestSig lbs) <- receive gw $ Sel [PingRequestSig]
    let request  = decode lbs
        response = mkTimestampedPingResponse request
    printf "Got '%s' from '%s'\n" (show request) (show from)
    sendWithSelf gw from $ Signal PingResponseSig (encode response)
  
runPingServer _ = error "Run as PingServer <Gateway address> <Gateway port>"