module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import GatewayProperties

main :: IO ()
main = defaultMain suite

suite :: [Test]
suite = 
  [ testGroup "Gateway properties"
    [ testProperty "Shall encode/decode message codes" prop_messageCodes
    ]
  ]