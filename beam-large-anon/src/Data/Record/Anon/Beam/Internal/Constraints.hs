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

-- | Constraint utilities and GHC.Generic interop for anonymous beam tables.
module Data.Record.Anon.Beam.Internal.Constraints (
    anonWithConstraints
  , anonWithConstrainedFields
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Proxy
import Database.Beam.Schema.Tables

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon

import Data.Record.Anon.Beam.Internal.ZipTables

{-------------------------------------------------------------------------------
  Constraint evidence
-------------------------------------------------------------------------------}

anonWithConstraints ::
     forall r c.
     AllFields r c
  => AnonTable r (HasConstraint c)
anonWithConstraints =
    AnonTable $ (Anon.cpure (Proxy @c) HasConstraint :: Record (HasConstraint c) r)

anonWithConstrainedFields ::
     forall r c.
     ( KnownFields r
     , AllFields r c
     )
  => AnonTable r Identity
  -> AnonTable r (WithConstraint c)
anonWithConstrainedFields (AnonTable rec) =
    AnonTable $
      Anon.czipWith
        (Proxy @c)
        (\HasConstraint (Identity v) -> WithConstraint v)
        (Anon.cpure (Proxy @c) HasConstraint :: Record (HasConstraint c) r)
        rec

{-------------------------------------------------------------------------------
  GHC.Generic interop (GFieldsFulfillConstraint)
-------------------------------------------------------------------------------}

instance AllFields r c
      => GFieldsFulfillConstraint c (AnonTableRep r Exposed) (AnonTableRep r (HasConstraint c)) where
  gWithConstrainedFields _ _ = AnonTableRep (anonWithConstraints @r @c)
