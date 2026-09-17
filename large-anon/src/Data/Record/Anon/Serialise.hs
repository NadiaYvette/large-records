{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CBOR serialisation support for anonymous records via 'Serialise'
module Data.Record.Anon.Serialise (
    -- * Re-exports
    Serialise(..)
    -- * Generic codecs
  , gencode
  , gdecode
  ) where

import Codec.Serialise (Serialise(..))
import Data.Record.Generic.Serialise (gencode, gdecode)
