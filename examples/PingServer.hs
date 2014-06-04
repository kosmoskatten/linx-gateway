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

-- | Simple ping server that just turns ping requests back to the
-- requester as a ping response.
main :: IO ()
main = runPingServer =<< getArgs

runPingServer :: [String] -> IO ()
runPingServer [gateway, port] = do
  -- Register itself in the gateway. Getting a Gateway instance in
  -- return.
  gw     <- create "server" gateway (Service port)
  -- Ask the gateway server to tell its configured name.
  gwName <- askName gw
  printf "PingServer now connectected to gateway '%s'\n" gwName
  
  -- Forever wait for signals of type ping request ...
  forever $ do
    (from, Signal PingRequestSig lbs) <- receive gw $ Sel [PingRequestSig]
    let request  = decode lbs -- Decode user level payload
        response = mkTimestampedPingResponse request
    printf "Got '%s' from '%s'\n" (show request) (show from)
    
    -- Send the response back to the requester.
    sendWithSelf gw from $ Signal PingResponseSig (encode response)
  
runPingServer _ = error "Run as PingServer <Gateway address> <Gateway port>"