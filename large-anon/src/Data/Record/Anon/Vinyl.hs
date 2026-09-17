{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Bidirectional conversion between anonymous records and vinyl extensible records
module Data.Record.Anon.Vinyl (
    -- * Conversion for advanced records
    toVinyl
  , fromVinyl
    -- * Conversion for simple records
  , toVinylSimple
  , fromVinylSimple
    -- * Vinyl re-exports
  , FieldRec
  , Rec(..)
  , ElField(..)
  , ToVinyl
  , FromVinyl
  ) where

import Data.Kind (Type)
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Functor (ElField(..))

import Data.Record.Generic.Vinyl (
    ToVinyl
  , FromVinyl
  , toVinylGeneric
  , fromVinylGeneric
  )

import Data.Record.Anon (FieldTypes, SimpleFieldTypes)
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Simple as Simple
import Data.Record.Anon.Plugin.Internal.Runtime (KnownFields, Row)

-- | A Vinyl record of fields
type FieldRec ts = Rec ElField ts

-- | Convert an advanced anonymous record to a Vinyl 'FieldRec'
toVinyl ::
     forall k (f :: k -> Type) (r :: Row k).
     (KnownFields r, ToVinyl (FieldTypes f r))
  => Record f r
  -> FieldRec (FieldTypes f r)
toVinyl = toVinylGeneric

-- | Convert a Vinyl 'FieldRec' to an advanced anonymous record
fromVinyl ::
     forall k (f :: k -> Type) (r :: Row k).
     (KnownFields r, FromVinyl (FieldTypes f r))
  => FieldRec (FieldTypes f r)
  -> Record f r
fromVinyl = fromVinylGeneric

-- | Convert a simple anonymous record to a Vinyl 'FieldRec'
toVinylSimple ::
     forall (r :: Row Type).
     (KnownFields r, ToVinyl (SimpleFieldTypes r))
  => Simple.Record r
  -> FieldRec (SimpleFieldTypes r)
toVinylSimple = toVinylGeneric

-- | Convert a Vinyl 'FieldRec' to a simple anonymous record
fromVinylSimple ::
     forall (r :: Row Type).
     (KnownFields r, FromVinyl (SimpleFieldTypes r))
  => FieldRec (SimpleFieldTypes r)
  -> Simple.Record r
fromVinylSimple = fromVinylGeneric
