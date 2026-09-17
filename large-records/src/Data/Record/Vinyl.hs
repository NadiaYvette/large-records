-- | Vinyl extensible record conversion for large records
module Data.Record.Vinyl (
    -- * Vinyl conversion
    toVinylGeneric
  , fromVinylGeneric
  , ToVinyl(..)
  , FromVinyl(..)
  ) where

import Data.Record.Generic.Vinyl
