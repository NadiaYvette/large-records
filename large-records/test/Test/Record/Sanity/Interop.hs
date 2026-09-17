{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Record.Sanity.Interop (tests) where

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (Serialise(..), serialise, deserialiseOrFail)
import Data.Aeson ()
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import Optics.Core (view, set, review)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic
import Data.Record.CBOR
import Data.Record.JSON
import Data.Record.Optics
import Data.Record.Protobuf
import Data.Record.Serialise
import Data.Record.Vinyl

{-------------------------------------------------------------------------------
  Test record defined with large-records
-------------------------------------------------------------------------------}

{-# ANN type Person largeRecord #-}
data Person = MkPerson {
      personName   :: String
    , personAge    :: Int
    , personActive :: Bool
    }
  deriving (Eq, Show)

instance Serialise Person where
  encode = gencode
  decode = gdecode

examplePerson :: Person
examplePerson = MkPerson "Bob" 42 True

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Interop" [
      testCase "Serialise_Array" test_serialise_array
    , testCase "Serialise_Map"   test_serialise_map
    , testCase "CBOR"            test_cbor
    , testCase "JSON"            test_json
    , testCase "Protobuf"        test_protobuf
    , testCase "Optics_Iso"      test_optics_iso
    , testCase "Optics_Lens"     test_optics_lens
    , testCase "Optics_Label"    test_optics_label
    , testCase "Vinyl"           test_vinyl
    ]

test_serialise_array :: Assertion
test_serialise_array = do
    let bytes = serialise examplePerson
    case deserialiseOrFail bytes of
      Left err -> assertFailure (show err)
      Right decoded -> assertEqual "roundtrip" examplePerson decoded

test_serialise_map :: Assertion
test_serialise_map = do
    let bytes = toStrictByteString (gencodeMap examplePerson)
    case deserialiseFromBytes gdecodeMap (LBS.fromStrict bytes) of
      Left err -> assertFailure (show err)
      Right (rest, decoded) -> do
        assertEqual "rest" "" rest
        assertEqual "roundtrip" examplePerson decoded

test_cbor :: Assertion
test_cbor = do
    let opts = defaultCBOROptions { cborFormat = CBORMap }
        bytes = toStrictByteString (gencodeCBORWith opts examplePerson)
    case deserialiseFromBytes (gdecodeCBORWith opts) (LBS.fromStrict bytes) of
      Left err -> assertFailure (show err)
      Right (rest, decoded) -> do
        assertEqual "rest" "" rest
        assertEqual "roundtrip" examplePerson decoded

test_json :: Assertion
test_json = do
    let opts = defaultJSONOptions
        jsonVal = gtoJSONWith opts examplePerson
    case Aeson.parseEither (gparseJSONWith opts) jsonVal of
      Left err -> assertFailure err
      Right decoded -> assertEqual "roundtrip" examplePerson decoded

test_protobuf :: Assertion
test_protobuf = do
    let bytes = gencodeProtobuf examplePerson
    case gdecodeProtobuf bytes of
      Left err -> assertFailure err
      Right decoded -> assertEqual "roundtrip" examplePerson decoded

test_optics_iso :: Assertion
test_optics_iso = do
    let repVal = view genericIso examplePerson
        back = review genericIso repVal
    assertEqual "roundtrip" examplePerson back

test_optics_lens :: Assertion
test_optics_lens = do
    let repVal = view genericLens examplePerson
        back = set genericLens repVal examplePerson
    assertEqual "view" examplePerson (to repVal)
    assertEqual "set" examplePerson back

test_optics_label :: Assertion
test_optics_label = do
    assertEqual "get personAge" 42 (view #personAge examplePerson)
    let modified = set #personAge 99 examplePerson
    assertEqual "set personAge" 99 (view #personAge modified)
    assertEqual "preserved personName" "Bob" (view #personName modified)

test_vinyl :: Assertion
test_vinyl = do
    let vRec = toVinylGeneric examplePerson
        back = fromVinylGeneric vRec
    assertEqual "roundtrip" examplePerson back
