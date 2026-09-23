{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | 'FromBackendRow' instance and GHC.Generic interop for anonymous tables.
module Data.Record.Anon.Beam.Internal.FromBackendRow (
    AnonFromBackendRow
  ) where

import Data.Functor.Identity (Identity(..))
import Data.List (foldl')
import Data.Proxy
import Database.Beam.Backend
import Database.Beam.Backend.SQL.Row
import Database.Beam.Schema.Tables

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import Data.Record.Generic (Dict(..))
import qualified Data.Record.Anon.Advanced as Anon

import Data.Record.Anon.Beam.Internal.ZipTables

{-------------------------------------------------------------------------------
  Constraint synonym
-------------------------------------------------------------------------------}

type AnonFromBackendRow be r = AllFields r (FromBackendRow be)

{-------------------------------------------------------------------------------
  GHC.Generic interop (GFromBackendRow)

  We do not provide an overlapping 'FromBackendRow be (AnonTable r Identity)'
  instance. Instead, we hook into beam's default 'FromBackendRow' by providing
  the generic 'GFromBackendRow' for our custom 'AnonTableRep'.
-------------------------------------------------------------------------------}

instance ( BeamBackend be
         , KnownFields r
         , AllFields r (FromBackendRow be)
         )
      => GFromBackendRow be (AnonTableRep r Exposed) (AnonTableRep r Identity) where

  gFromBackendRow _ = do
      rec <- Anon.cmapM
               (Proxy @(FromBackendRow be))
               (\Dict -> Identity <$> fromBackendRow)
               (Anon.reifyAllFields (Proxy @(FromBackendRow be)) :: Record (Dict (FromBackendRow be)) r)
      return (AnonTableRep (AnonTable rec))

  gValuesNeeded pBe _ _ =
      foldl' (+) 0 $
        Anon.collapse $
          Anon.cmap
            (Proxy @(FromBackendRow be))
            (\(Dict :: Dict (FromBackendRow be) a) -> K (valuesNeeded pBe (Proxy @a)))
            (Anon.reifyAllFields (Proxy @(FromBackendRow be)) :: Record (Dict (FromBackendRow be)) r)
