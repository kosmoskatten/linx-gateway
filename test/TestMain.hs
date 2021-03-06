module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import MessageProperties
import SignalProperties

main :: IO ()
main = defaultMain suite

suite :: [Test]
suite = 
  [ testGroup "Message properties"
    [ testProperty "Shall encode/decode messages" prop_message
    ]
  , testGroup "Signal properties"
    [ testProperty "Shall encode/decode signals" prop_signal
    , testProperty "Shall encode/decode signalSelectors" prop_signalSelector
    ]
  ]