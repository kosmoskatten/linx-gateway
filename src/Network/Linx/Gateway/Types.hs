{-# LANGUAGE DeriveGeneric #-}
module Network.Linx.Gateway.Types
       ( Status (..)
       , Length (..)
       , Version (..)
       , Flags (..)
       , CString (..)
       , User (..)
       , Pid (..)
       , mkCString
       ) where

import Control.Applicative ((<$>))
import Data.Binary
import Data.Binary.Get (getLazyByteStringNul)
import Data.Binary.Put (putLazyByteString)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.Linx.Gateway.BinaryInt32 (Int32, getInt32, putInt32)

-- | Status indicator           
data Status =
    Success
  | Error
  deriving (Show, Eq)
           
-- | Length descriptor.
newtype Length = Length Int32
  deriving (Show, Eq, Generic)
           
-- | Version descriptor.
data Version =
    V100
  | Version !Int32
  deriving (Show, Eq)
           
-- | Flags descriptor.
data Flags =
    BigEndian
  | LittleEndian
  | Flags !Int32
  deriving (Show, Eq)
           
-- | Null terminated C-string.
newtype CString = CString LBS.ByteString
  deriving (Show, Eq)

-- | User identifier (always zero).
data User = AlwaysZero
  deriving (Show, Eq)
           
-- | Process identifier for a Linx process.
newtype Pid = Pid Int32
  deriving (Show, Eq, Generic)
           
-- | Generic binary instances.
instance Binary Length
instance Binary Pid

-- | Binary instance for 'Status'.
instance Binary Status where
  get = do
    value <- getInt32
    return $
      case value of
        0 -> Success
        _ -> Error
        
  put Success = putInt32 0
  put Error   = putInt32 (-1)

-- | Binary instance for 'Version'.
instance Binary Version where
  get = do
    value <- getInt32
    return $
      case value of
        100 -> V100
        _   -> Version value
  
  put V100            = putInt32 100
  put (Version value) = put value
        
-- | Binary instance for 'Flags'.
instance Binary Flags where
  get = do
    value <- getInt32
    return $
      case value of
        0 -> BigEndian
        1 -> LittleEndian
        _ -> Flags value
        
  put BigEndian     = putInt32 0
  put LittleEndian  = putInt32 1
  put (Flags value) = put value

-- | Binary instance for 'CString'.
instance Binary CString where
  get = CString <$> getLazyByteStringNul
  put (CString lbs) = putLazyByteString lbs >> putWord8 0
  
-- | Binary instance for 'User'.
instance Binary User where
  get = do
    value <- getInt32
    return $
      case value of
        0 -> AlwaysZero
        _ -> error $ "Unexpected user value: " ++ show value
  
  put AlwaysZero = putInt32 0
  
-- | Make a CString.
mkCString :: String -> CString
mkCString = CString . LBS.pack