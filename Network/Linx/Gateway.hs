module Network.Linx.Gateway
       ( MessageCode (..)
       , encode
       , decode
       ) where

import Data.Binary
import Data.Int (Int32)

-- | Message codes describing the identities for requests and
-- replies. Not implementing codes marked as 'Not used' in the
-- protocol specification.
data MessageCode =
    InterfaceRequest
  | InterfaceReply
  | CreateRequest
  | CreateReply
  | DestroyRequest
  | DestroyReply
  deriving (Show, Eq)
           
-- | Binary instance for 'MessageCode'.
instance Binary MessageCode where
  put InterfaceRequest  = putInt32 1
  put InterfaceReply    = putInt32 2
  put CreateRequest     = putInt32 7
  put CreateReply       = putInt32 8
  put DestroyRequest    = putInt32 9
  put DestroyReply      = putInt32 10
  get                   = do
    value <- get :: Get Int32
    case value of
      1  -> return InterfaceRequest
      2  -> return InterfaceReply
      7  -> return CreateRequest
      8  -> return CreateReply
      9  -> return DestroyRequest
      10 -> return DestroyReply
      _  -> error "Unexpected binary message code"
      
putInt32 :: Int32 -> Put
putInt32 = put