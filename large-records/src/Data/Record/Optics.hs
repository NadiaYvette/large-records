-- | Optics integration for large records
module Data.Record.Optics (
    -- * Generic Optics
    genericIso
  , genericLens
    -- * Conversion to and from van Laarhoven lenses
  , vlToLens
  , lensToVL
  ) where

import Data.Record.Generic.Optics
