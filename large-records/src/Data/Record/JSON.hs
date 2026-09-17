-- | JSON options and codecs for large records
module Data.Record.JSON (
    -- * Options
    JSONOptions(..)
  , defaultJSONOptions
    -- * Generic codecs
  , gtoJSON
  , gparseJSON
  , gtoJSONWith
  , gparseJSONWith
  , gtoEncoding
  , gtoPairs
  ) where

import Data.Record.Generic.JSON
