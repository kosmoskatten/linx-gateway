module MessageProperties where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Binary
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as LBS
import Test.QuickCheck hiding (Success)
import Network.Linx.Gateway.Message


prop_message :: Bool
prop_message = True