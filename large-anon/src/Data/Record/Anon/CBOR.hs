{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | CBOR low-level serialization and options for anonymous records
module Data.Record.Anon.CBOR (
    -- * Options
    CBORFormat(..)
  , CBOROptions(..)
  , defaultCBOROptions
    -- * Codecs for advanced records
  , encodeRecordArray
  , decodeRecordArray
  , encodeRecordMap
  , decodeRecordMap
    -- * CBOR encoding/decoding
  , encodeRecordCBOR
  , decodeRecordCBOR
  , encodeSimpleRecordCBOR
  , decodeSimpleRecordCBOR
  ) where

import Codec.CBOR.Decoding (Decoder, decodeListLen)
import Codec.CBOR.Encoding (Encoding, encodeListLen, encodeMapLen, encodeString)
import Codec.Serialise (Serialise)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Data.SOP.BasicFunctors ((:.:)(..), I(..), K(..))

import Data.Record.Generic.CBOR (CBORFormat(..), CBOROptions(..), defaultCBOROptions, gencodeCBORWith, gdecodeCBORWith)

import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Advanced
import qualified Data.Record.Anon.Simple as Simple
import Data.Record.Anon.Internal.Advanced (RecordConstraints)
import qualified Data.Record.Anon.Internal.Simple as SimpleInternal
import Data.Record.Anon.Plugin.Internal.Runtime (KnownFields, Row)

-- | Encode record fields as a CBOR array
encodeRecordArray ::
     forall k (f :: k -> Type) (r :: Row k).
     (forall x. f x -> Encoding)
  -> Record f r
  -> Encoding
encodeRecordArray enc r =
    let fields = Advanced.collapse (Advanced.map (K . enc) r)
    in encodeListLen (fromIntegral (length fields)) <> mconcat fields

-- | Decode record fields from a CBOR array
decodeRecordArray ::
     forall s (r :: Row Type).
     KnownFields r
  => Advanced.Record (Decoder s) r
  -> Decoder s (Simple.Record r)
decodeRecordArray decoders = do
    len <- decodeListLen
    let expected = length (Advanced.collapse (Advanced.reifyKnownFields (Proxy @r)))
    if len /= expected
      then fail $ "decodeRecordArray: expected array of length " ++ show expected ++ ", got " ++ show len
      else Simple.fromAdvanced <$> Advanced.sequenceA (Advanced.map (Comp . fmap I) decoders)

-- | Encode record fields as a CBOR map with field names as keys
encodeRecordMap ::
     forall k (f :: k -> Type) (r :: Row k).
     KnownFields r
  => (String -> String)
  -> (forall x. f x -> Encoding)
  -> Record f r
  -> Encoding
encodeRecordMap labelMod enc r =
    let pairs = Advanced.collapse $
                  Advanced.zipWith
                    (\(K name) fx -> K (encodeString (T.pack (labelMod name)) <> enc fx))
                    (Advanced.reifyKnownFields (Proxy @r))
                    r
    in encodeMapLen (fromIntegral (length pairs)) <> mconcat pairs

-- | Decode record fields from a CBOR map using Serialise instances
decodeRecordMap ::
     forall s r.
     SimpleInternal.RecordConstraints r Serialise
  => (String -> String)
  -> CBOROptions
  -> Decoder s (Simple.Record r)
decodeRecordMap labelMod opts =
    decodeSimpleRecordCBOR (opts { cborFormat = CBORMap, cborFieldLabelModifier = labelMod })

-- | Encode advanced record using CBOR options
encodeRecordCBOR ::
     forall k (f :: k -> Type) (r :: Row k).
     RecordConstraints f r Serialise
  => CBOROptions
  -> Record f r
  -> Encoding
encodeRecordCBOR = gencodeCBORWith

-- | Decode advanced record using CBOR options
decodeRecordCBOR ::
     forall k s (f :: k -> Type) (r :: Row k).
     RecordConstraints f r Serialise
  => CBOROptions
  -> Decoder s (Record f r)
decodeRecordCBOR = gdecodeCBORWith

-- | Encode simple record using CBOR options
encodeSimpleRecordCBOR ::
     forall r.
     SimpleInternal.RecordConstraints r Serialise
  => CBOROptions
  -> Simple.Record r
  -> Encoding
encodeSimpleRecordCBOR = gencodeCBORWith

-- | Decode simple record using CBOR options
decodeSimpleRecordCBOR ::
     forall s r.
     SimpleInternal.RecordConstraints r Serialise
  => CBOROptions
  -> Decoder s (Simple.Record r)
decodeSimpleRecordCBOR = gdecodeCBORWith
