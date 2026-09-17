{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Profunctor optics integration for anonymous records
module Data.Record.Anon.Optics (
    -- * Sub-record projection optics
    projectOptic
  , projectSimpleOptic
    -- * Field access optics
  , fieldOptic
  , fieldSimpleOptic
    -- * Optics utilities
  , genericIso
    -- * Re-exports
  , Lens
  , Lens'
  , Iso
  , Iso'
  , (%)
  , view
  , set
  , over
  ) where

import Data.Kind (Type)
import Optics.Core (
    Iso
  , Iso'
  , Lens
  , Lens'
  , (%)
  , iso
  , lens
  , over
  , set
  , view
  )

import Data.Record.Generic (Generic(..), Rep, I)
import Data.Record.Anon (RowHasField, SubRow, Field)
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Advanced
import qualified Data.Record.Anon.Simple as Simple
import Data.Record.Anon.Plugin.Internal.Runtime (Row)

-- | Sub-record lens for advanced records
projectOptic ::
     forall k (f :: k -> Type) (r :: Row k) (r' :: Row k).
     SubRow r r'
  => Lens' (Record f r) (Record f r')
projectOptic = lens Advanced.project (\big small -> Advanced.inject small big)

-- | Sub-record lens for simple records
projectSimpleOptic ::
     forall (r :: Row Type) (r' :: Row Type).
     SubRow r r'
  => Lens' (Simple.Record r) (Simple.Record r')
projectSimpleOptic = lens Simple.project (\big small -> Simple.inject small big)

-- | Field lens for advanced records
fieldOptic ::
     forall n r a f.
     RowHasField n r a
  => Field n
  -> Lens' (Record f r) (f a)
fieldOptic fld = lens (Advanced.get fld) (\r val -> Advanced.set fld val r)

-- | Field lens for simple records
fieldSimpleOptic ::
     forall n r a.
     RowHasField n r a
  => Field n
  -> Lens' (Simple.Record r) a
fieldSimpleOptic fld = lens (Simple.get fld) (\r val -> Simple.set fld val r)

-- | Generic isomorphism between an anonymous record and its internal 'Rep'
genericIso :: Generic a => Iso' a (Rep I a)
genericIso = iso from to
