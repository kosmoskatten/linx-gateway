module Network.Linx.Gateway.BinaryList
       ( putList
       , getList
       ) where

import Control.Monad (replicateM)
import Data.Binary
import Network.Linx.Gateway.Types

putList :: Binary a => [a] -> Put
putList = mapM_ put

getList :: Binary a => Length -> Get [a]
getList len = replicateM (asInt len) get