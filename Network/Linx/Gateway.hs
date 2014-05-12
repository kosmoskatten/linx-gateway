module Network.Linx.Gateway
       ( MessageCode (..)
       , encode
       , decode
       ) where

import Data.Binary
import Data.Int (Int32)

-- | Message codes describing the identities for requests and replies.
data MessageCode =
    InterfaceRequest
  | InterfaceReply
  deriving (Show, Eq)
           
-- | Binary instance for 'MessageCode'.
instance Binary MessageCode where
  put InterfaceRequest = putInt32 1
  put InterfaceReply   = putInt32 2
  get                  = do
    value <- get :: Get Int32
    case value of
      1 -> return InterfaceRequest
      2 -> return InterfaceReply
      _ -> error "Unexpected binary message code"
      
putInt32 :: Int32 -> Put
putInt32 = put