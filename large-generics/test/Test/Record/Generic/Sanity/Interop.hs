{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Record.Generic.Sanity.Interop (tests) where

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (Serialise(..), serialise, deserialiseOrFail)
import qualified Data.ByteString.Lazy as LBS
import Optics.Core (view, set, review)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic
import Data.Record.Generic.CBOR
import Data.Record.Generic.JSON
import Data.Record.Generic.Optics
import Data.Record.Generic.Protobuf
import Data.Record.Generic.Serialise
import Data.Record.Generic.Vinyl
import Test.Record.Generic.Infra.Examples

import Data.Aeson.Types (parseEither)
import Data.Vinyl.Core (Rec(..))

instance Serialise SimpleRecord where
  encode = gencode
  decode = gdecode

tests :: TestTree
tests = testGroup "Test.Record.Generic.Sanity.Interop" [
      testCase "Serialise_Array"      test_serialise_array
    , testCase "Serialise_Map"        test_serialise_map
    , testCase "CBOR_Array"           test_cbor_array
    , testCase "CBOR_Map"             test_cbor_map
    , testCase "Protobuf"             test_protobuf
    , testCase "Optics_Iso"           test_optics_iso
    , testCase "Optics_Lens"          test_optics_lens
    , testCase "Optics_VL_Conversion" test_optics_vl
    , testCase "Vinyl"                test_vinyl
    , testCase "JSON_Options"         test_json_options
    ]

test_serialise_array :: Assertion
test_serialise_array = do
    let orig = exampleSimpleRecord
        bytes = serialise orig
    case deserialiseOrFail bytes of
      Left err -> assertFailure (show err)
      Right decoded -> assertEqual "roundtrip" orig decoded

test_serialise_map :: Assertion
test_serialise_map = do
    let orig = exampleSimpleRecord
        bytes = toStrictByteString (gencodeMap orig)
    case deserialiseFromBytes gdecodeMap (LBS.fromStrict bytes) of
      Left err -> assertFailure (show err)
      Right (rest, decoded) -> do
        assertEqual "rest" "" rest
        assertEqual "roundtrip" orig decoded

test_cbor_array :: Assertion
test_cbor_array = do
    let orig = exampleSimpleRecord
        opts = defaultCBOROptions { cborFormat = CBORArray }
        bytes = toStrictByteString (gencodeCBORWith opts orig)
    case deserialiseFromBytes (gdecodeCBORWith opts) (LBS.fromStrict bytes) of
      Left err -> assertFailure (show err)
      Right (rest, decoded) -> do
        assertEqual "rest" "" rest
        assertEqual "roundtrip" orig decoded

test_cbor_map :: Assertion
test_cbor_map = do
    let orig = exampleSimpleRecord
        opts = defaultCBOROptions {
            cborFormat = CBORMap
          , cborFieldLabelModifier = ("test_" ++)
          }
        bytes = toStrictByteString (gencodeCBORWith opts orig)
    case deserialiseFromBytes (gdecodeCBORWith opts) (LBS.fromStrict bytes) of
      Left err -> assertFailure (show err)
      Right (rest, decoded) -> do
        assertEqual "rest" "" rest
        assertEqual "roundtrip" orig decoded

test_protobuf :: Assertion
test_protobuf = do
    let orig = exampleSimpleRecord
        encoded = gencodeProtobuf orig
    case gdecodeProtobuf encoded of
      Left err -> assertFailure err
      Right decoded -> assertEqual "roundtrip" orig decoded

test_optics_iso :: Assertion
test_optics_iso = do
    let orig = exampleSimpleRecord
        repVal = view genericIso orig
        back = review genericIso repVal
    assertEqual "roundtrip" orig back

test_optics_lens :: Assertion
test_optics_lens = do
    let orig = exampleSimpleRecord
        repVal = view genericLens orig
        back = set genericLens repVal orig
    assertEqual "genericLens view" orig (to repVal)
    assertEqual "genericLens set" orig back

test_optics_vl :: Assertion
test_optics_vl = do
    let orig = exampleSimpleRecord
        vl1 f s = (\x -> s { simpleRecordField1 = x }) <$> f (simpleRecordField1 s)
        l1 = vlToLens vl1
    assertEqual "vlToLens view" (simpleRecordField1 orig) (view l1 orig)
    assertEqual "vlToLens set" 123 (simpleRecordField1 (set l1 123 orig))

test_vinyl :: Assertion
test_vinyl = do
    let orig = exampleSimpleRecord
        vRec = toVinylGeneric orig
        back = fromVinylGeneric vRec
    case vRec of
      _ :& _ -> return ()
    assertEqual "roundtrip" orig back

test_json_options :: Assertion
test_json_options = do
    let orig = exampleSimpleRecord
        opts = defaultJSONOptions { jsonFieldLabelModifier = ("json_" ++) }
        val = gtoJSONWith opts orig
    case parseEither (gparseJSONWith opts) val of
      Left err -> assertFailure err
      Right decoded -> assertEqual "roundtrip" orig decoded
