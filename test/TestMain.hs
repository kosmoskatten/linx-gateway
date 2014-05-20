module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import MessageProperties

main :: IO ()
main = defaultMain suite

suite :: [Test]
suite = 
  [ testGroup "Message properties"
    [ testProperty "Shall encode/decode messages" prop_message
    ]
  ]