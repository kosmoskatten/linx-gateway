module Main where

import Control.Monad (forever)
import System.Environment (getArgs)
import Network.Linx.Gateway
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
    (from, Signal _ lbs) <- receive gw $ Sel [pingRequestSig]
    let request  = decode lbs
        response = mkTimestampedPingResponse request
    printf "Got '%s' from '%s'\n" (show request) (show from)
    sendWithSelf gw from $ Signal pingResponseSig (encode response)
  
runPingServer _ = error "Run as PingServer <Gateway address> <Gateway port>"