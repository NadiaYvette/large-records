{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.Interop (tests) where

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (deserialiseOrFail, serialise)
import Data.Aeson (object, (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.List (isInfixOf)
import qualified Options.Applicative as OA
import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon
import qualified Data.Record.Anon.Advanced as Advanced
import qualified Data.Record.Anon.Simple as Anon
import Data.Record.Anon.CBOR
import Data.Record.Anon.DocRecord
import Data.Record.Anon.Optics
import Data.Record.Anon.Protobuf
import Data.Record.Anon.Serialise ()
import Data.Record.Anon.Soup
import Data.Record.Anon.Vinyl

tests :: TestTree
tests = testGroup "Test.Sanity.Interop" [
      testCase "Serialise"  test_serialise
    , testCase "CBOR"       test_cbor
    , testCase "Protobuf"   test_protobuf
    , testCase "Optics"     test_optics
    , testCase "Vinyl"      test_vinyl
    , testCase "DocRecord"  test_docrecord
    , testCase "Soup"       test_soup
    ]

type SampleRow = '[ "age" := Int, "name" := String ]

sampleRec :: Anon.Record SampleRow
sampleRec =
      Anon.insert #age 30 $
      Anon.insert #name "Alice" $
      Anon.empty

test_serialise :: Assertion
test_serialise = do
    let bytes = serialise sampleRec
    case deserialiseOrFail bytes of
      Left err -> assertFailure (show err)
      Right decoded -> assertEqual "roundtrip" sampleRec (decoded :: Anon.Record SampleRow)

test_cbor :: Assertion
test_cbor = do
    let optsArr = defaultCBOROptions { cborFormat = CBORArray }
        optsMap = defaultCBOROptions { cborFormat = CBORMap }
        bytesArr = toStrictByteString (encodeSimpleRecordCBOR optsArr sampleRec)
        bytesMap = toStrictByteString (encodeSimpleRecordCBOR optsMap sampleRec)
    case deserialiseFromBytes (decodeSimpleRecordCBOR optsArr) (LBS.fromStrict bytesArr) of
      Left err -> assertFailure (show err)
      Right (rest, decoded) -> do
        assertEqual "rest empty" "" rest
        assertEqual "roundtrip array" sampleRec decoded
    case deserialiseFromBytes (decodeSimpleRecordCBOR optsMap) (LBS.fromStrict bytesMap) of
      Left err -> assertFailure (show err)
      Right (rest, decoded) -> do
        assertEqual "rest empty" "" rest
        assertEqual "roundtrip map" sampleRec decoded

test_protobuf :: Assertion
test_protobuf = do
    let bytes = encodeSimpleProtobuf sampleRec
    case decodeSimpleProtobuf bytes of
      Left err -> assertFailure err
      Right decoded -> assertEqual "roundtrip protobuf" sampleRec decoded

test_optics :: Assertion
test_optics = do
    let optAge = fieldSimpleOptic #age
    assertEqual "view field" 30 (view optAge sampleRec)
    assertEqual "set field" 31 (view optAge (set optAge 31 sampleRec))

    let subOpt = projectSimpleOptic @_ @'[ "age" := Int ]
        subRec = Anon.insert #age 30 Anon.empty
    assertEqual "view subrecord" subRec (view subOpt sampleRec)
    let modified = set subOpt (Anon.insert #age 99 Anon.empty) sampleRec
    assertEqual "set subrecord field age" 99 (Anon.get #age modified)
    assertEqual "set subrecord field name" "Alice" (Anon.get #name modified)

test_vinyl :: Assertion
test_vinyl = do
    let vRec = toVinylSimple sampleRec
        back = fromVinylSimple vRec
    assertEqual "roundtrip vinyl" sampleRec back

test_docrecord :: Assertion
test_docrecord = do
    let docRec :: DocRecord '[ "port" := Int, "host" := String ]
        docRec =
          Advanced.insert #port (docDef "Port number" 8080) $
          Advanced.insert #host (doc "Host name") $
          Advanced.empty

    -- Schema & markdown
    let schema = docRecordSchema docRec
    assertEqual "schema length" 2 (length schema)
    let md = docRecordMarkdown docRec
    assertBool "markdown contains Port number" ("Port number" `isInfixOf` md)

    -- Resolve missing required field
    case resolveDocRecord docRec of
      Left _ -> return ()
      Right _ -> assertFailure "Should fail because host is missing"

    -- Resolve with value provided
    let docRecProvided :: DocRecord '[ "port" := Int, "host" := String ]
        docRecProvided =
          Advanced.insert #port (docDef "Port number" 8080) $
          Advanced.insert #host (docVal "Host name" ("localhost" :: String)) $
          Advanced.empty
    case resolveDocRecord docRecProvided of
      Left err -> assertFailure err
      Right resolved -> do
        assertEqual "port" 8080 (Anon.get #port resolved)
        assertEqual "host" "localhost" (Anon.get #host resolved)

    -- JSON parsing with defaults
    let jsonVal = object ["host" .= ("127.0.0.1" :: String)]
    case parseEither (parseJSONDocRecord docRec) jsonVal of
      Left err -> assertFailure err
      Right r -> do
        assertEqual "port default" 8080 (Anon.get #port r)
        assertEqual "host parsed" "127.0.0.1" (Anon.get #host r)

    -- CLI parser with optparse-applicative
    let p = docRecordParser docRec
        pInfo = OA.info p OA.idm
        execArgs args = OA.execParserPure OA.defaultPrefs pInfo args
    case execArgs ["--host", show ("example.com" :: String)] of
      OA.Success r -> do
        assertEqual "cli port default" 8080 (Anon.get #port r)
        assertEqual "cli host parsed" "example.com" (Anon.get #host r)
      OA.Failure f -> assertFailure (show f)
      OA.CompletionInvoked _ -> assertFailure "completion invoked"

type Env = '[ "host" := String, "port" := Int, "verbose" := Bool ]

test_soup :: Assertion
test_soup = do
    let env :: Anon.Record Env
        env =
          Anon.insert #host "localhost" $
          Anon.insert #port 8080 $
          Anon.insert #verbose True $
          Anon.empty

    let readHost :: Soup '[ "host" := String ] String
        readHost = askField #host

        combinedAction :: Soup Env (String, Int)
        combinedAction = do
          h <- zoomSoup readHost
          p <- askField #port
          return (h, p)

    let (resHost, resPort) = runSoup combinedAction env
    assertEqual "resHost" "localhost" resHost
    assertEqual "resPort" 8080 resPort

    let resPour = pour readHost env
    assertEqual "pour" "localhost" resPour
    let resPourRec = pourRecord env readHost
    assertEqual "pourRecord" "localhost" resPourRec
