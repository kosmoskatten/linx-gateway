module Network.Linx.Gateway.BinaryInt32
       ( Int32
       , getInt32
       , putInt32
       ) where

import Data.Binary
import Data.Int

getInt32 :: Get Int32
getInt32 = get
  
putInt32 :: Int32 -> Put
putInt32 = put