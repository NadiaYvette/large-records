module Main (main) where

import Test.Tasty

import qualified Test.Record.Anon.Beam.Tutorial1 as Tutorial1
import qualified Test.Record.Anon.Beam.Zipping   as Zipping

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "beam-large-anon" [
      Tutorial1.tests
    , Zipping.tests
    ]
