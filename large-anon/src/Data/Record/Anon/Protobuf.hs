{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | Protocol Buffers wire format serialization for anonymous records
module Data.Record.Anon.Protobuf (
    -- * Serialization
    encodeProtobuf
  , decodeProtobuf
  , encodeProtobufWith
  , decodeProtobufWith
  , encodeSimpleProtobuf
  , decodeSimpleProtobuf
  , encodeSimpleProtobufWith
  , decodeSimpleProtobufWith
    -- * Wire types and class
  , ProtobufField(..)
  , WireType(..)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Word (Word32)

import Data.Record.Generic (FieldMetadata, K)
import Data.Record.Generic.Protobuf (
    ProtobufField(..)
  , WireType(..)
  , gencodeProtobuf
  , gdecodeProtobuf
  , gencodeProtobufWith
  , gdecodeProtobufWith
  )

import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Simple as Simple
import Data.Record.Anon.Internal.Advanced (RecordConstraints)
import qualified Data.Record.Anon.Internal.Simple as SimpleInternal
import Data.Record.Anon.Plugin.Internal.Runtime (Row)

-- | Encode an advanced anonymous record to Protocol Buffers wire format
encodeProtobuf ::
     forall k (f :: k -> Type) (r :: Row k).
     RecordConstraints f r ProtobufField
  => Record f r
  -> ByteString
encodeProtobuf = gencodeProtobuf

-- | Decode an advanced anonymous record from Protocol Buffers wire format
decodeProtobuf ::
     forall k (f :: k -> Type) (r :: Row k).
     RecordConstraints f r ProtobufField
  => ByteString
  -> Either String (Record f r)
decodeProtobuf = gdecodeProtobuf

-- | Encode with custom tag assignment
encodeProtobufWith ::
     forall k (f :: k -> Type) (r :: Row k).
     RecordConstraints f r ProtobufField
  => (forall x. FieldMetadata x -> K Word32 x -> Word32)
  -> Record f r
  -> ByteString
encodeProtobufWith = gencodeProtobufWith

-- | Decode with custom tag assignment
decodeProtobufWith ::
     forall k (f :: k -> Type) (r :: Row k).
     RecordConstraints f r ProtobufField
  => (forall x. FieldMetadata x -> K Word32 x -> Word32)
  -> ByteString
  -> Either String (Record f r)
decodeProtobufWith = gdecodeProtobufWith

-- | Encode a simple anonymous record to Protocol Buffers wire format
encodeSimpleProtobuf ::
     forall r.
     SimpleInternal.RecordConstraints r ProtobufField
  => Simple.Record r
  -> ByteString
encodeSimpleProtobuf = gencodeProtobuf

-- | Decode a simple anonymous record from Protocol Buffers wire format
decodeSimpleProtobuf ::
     forall r.
     SimpleInternal.RecordConstraints r ProtobufField
  => ByteString
  -> Either String (Simple.Record r)
decodeSimpleProtobuf = gdecodeProtobuf

-- | Encode simple record with custom tag assignment
encodeSimpleProtobufWith ::
     forall r.
     SimpleInternal.RecordConstraints r ProtobufField
  => (forall x. FieldMetadata x -> K Word32 x -> Word32)
  -> Simple.Record r
  -> ByteString
encodeSimpleProtobufWith = gencodeProtobufWith

-- | Decode simple record with custom tag assignment
decodeSimpleProtobufWith ::
     forall r.
     SimpleInternal.RecordConstraints r ProtobufField
  => (forall x. FieldMetadata x -> K Word32 x -> Word32)
  -> ByteString
  -> Either String (Simple.Record r)
decodeSimpleProtobufWith = gdecodeProtobufWith
