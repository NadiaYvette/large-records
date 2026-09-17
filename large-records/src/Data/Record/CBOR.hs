-- | CBOR serialization options and codecs for large records
module Data.Record.CBOR (
    -- * Options
    CBOROptions(..)
  , CBORFormat(..)
  , defaultCBOROptions
    -- * Generic codecs
  , gencodeCBORWith
  , gdecodeCBORWith
  ) where

import Data.Record.Generic.CBOR
