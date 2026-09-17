{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | Enhanced JSON support for anonymous records with customizable options
module Data.Record.Anon.JSON (
    -- * Configuration
    JSONOptions(..)
  , defaultJSONOptions
    -- * Advanced records
  , toJSONWith
  , parseJSONWith
  , toEncodingWith
  , toPairsWith
    -- * Simple records
  , toJSONSimpleWith
  , parseJSONSimpleWith
  , toEncodingSimpleWith
  , toPairsSimpleWith
    -- * Re-exports
  , ToJSON(..)
  , FromJSON(..)
  , Value
  , Parser
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value)
import Data.Aeson.Encoding (Encoding)
import Data.Aeson.Types (Parser, Pair)
import Data.Kind (Type)

import Data.Record.Generic.JSON (
    JSONOptions(..)
  , defaultJSONOptions
  , gtoJSONWith
  , gparseJSONWith
  , gtoEncodingWith
  , gtoPairsWith
  )

import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Simple as Simple
import Data.Record.Anon.Internal.Advanced (RecordConstraints)
import qualified Data.Record.Anon.Internal.Simple as SimpleInternal
import Data.Record.Anon.Plugin.Internal.Runtime (Row)

-- | Encode advanced record to JSON 'Value' with options
toJSONWith ::
     forall k (f :: k -> Type) (r :: Row k).
     RecordConstraints f r ToJSON
  => JSONOptions
  -> Record f r
  -> Value
toJSONWith = gtoJSONWith

-- | Parse advanced record from JSON 'Value' with options
parseJSONWith ::
     forall k (f :: k -> Type) (r :: Row k).
     RecordConstraints f r FromJSON
  => JSONOptions
  -> Value
  -> Parser (Record f r)
parseJSONWith = gparseJSONWith

-- | Encode advanced record to JSON 'Encoding' (streaming) with options
toEncodingWith ::
     forall k (f :: k -> Type) (r :: Row k).
     RecordConstraints f r ToJSON
  => JSONOptions
  -> Record f r
  -> Encoding
toEncodingWith = gtoEncodingWith

-- | Convert advanced record to key-value 'Pair' list with options
toPairsWith ::
     forall k (f :: k -> Type) (r :: Row k).
     RecordConstraints f r ToJSON
  => JSONOptions
  -> Record f r
  -> [Pair]
toPairsWith = gtoPairsWith

-- | Encode simple record to JSON 'Value' with options
toJSONSimpleWith ::
     forall r.
     SimpleInternal.RecordConstraints r ToJSON
  => JSONOptions
  -> Simple.Record r
  -> Value
toJSONSimpleWith = gtoJSONWith

-- | Parse simple record from JSON 'Value' with options
parseJSONSimpleWith ::
     forall r.
     SimpleInternal.RecordConstraints r FromJSON
  => JSONOptions
  -> Value
  -> Parser (Simple.Record r)
parseJSONSimpleWith = gparseJSONWith

-- | Encode simple record to JSON 'Encoding' with options
toEncodingSimpleWith ::
     forall r.
     SimpleInternal.RecordConstraints r ToJSON
  => JSONOptions
  -> Simple.Record r
  -> Encoding
toEncodingSimpleWith = gtoEncodingWith

-- | Convert simple record to key-value 'Pair' list with options
toPairsSimpleWith ::
     forall r.
     SimpleInternal.RecordConstraints r ToJSON
  => JSONOptions
  -> Simple.Record r
  -> [Pair]
toPairsSimpleWith = gtoPairsWith
