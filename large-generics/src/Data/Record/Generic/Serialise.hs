{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generic conversion to/from 'Serialise'
module Data.Record.Generic.Serialise (
    -- * Standard generic serialization
    gencode
  , gdecode
    -- * Map-based serialization
  , gencodeMap
  , gdecodeMap
    -- * Configurable serialization
  , gencodeWith
  , gdecodeWith
    -- * Re-exports
  , CBORFormat(..)
  , CBOROptions(..)
  , defaultCBOROptions
  ) where

import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.Serialise.Class (Serialise(..))

import Data.Record.Generic
import Data.Record.Generic.CBOR

-- | Generic CBOR encoding as array
gencode :: (Generic a, Constraints a Serialise) => a -> Encoding
gencode = gencodeCBOR

-- | Generic CBOR decoding from array
gdecode :: (Generic a, Constraints a Serialise) => Decoder s a
gdecode = gdecodeCBOR

-- | Generic CBOR encoding as map (keyed by field name)
gencodeMap :: (Generic a, Constraints a Serialise) => a -> Encoding
gencodeMap = gencodeCBORWith (defaultCBOROptions { cborFormat = CBORMap })

-- | Generic CBOR decoding from map (keyed by field name, handles out-of-order keys)
gdecodeMap :: (Generic a, Constraints a Serialise) => Decoder s a
gdecodeMap = gdecodeCBORWith (defaultCBOROptions { cborFormat = CBORMap })

-- | Configurable generic encoding
gencodeWith :: (Generic a, Constraints a Serialise) => CBOROptions -> a -> Encoding
gencodeWith = gencodeCBORWith

-- | Configurable generic decoding
gdecodeWith :: (Generic a, Constraints a Serialise) => CBOROptions -> Decoder s a
gdecodeWith = gdecodeCBORWith
