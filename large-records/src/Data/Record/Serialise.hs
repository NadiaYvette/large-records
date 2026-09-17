-- | Serialise (CBOR-based) generic codecs for large records
module Data.Record.Serialise (
    -- * Standard array-based encoding
    gencode
  , gdecode
    -- * Map-based encoding
  , gencodeMap
  , gdecodeMap
  ) where

import Data.Record.Generic.Serialise
