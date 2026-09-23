{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Core 'Beamable' instance and GHC.Generic interop for anonymous tables.
module Data.Record.Anon.Beam.Internal.ZipTables (
    AnonTable(..)
  , AnonTableRep(..)
  ) where

import Data.Coerce (coerce)
import Data.Kind
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)
import Database.Beam.Schema.Tables

import qualified GHC.Generics as GHC

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon

{-------------------------------------------------------------------------------
  The AnonTable wrapper
-------------------------------------------------------------------------------}

newtype AnonTable (r :: Row Type) (f :: Type -> Type) =
    AnonTable { unAnonTable :: Record f r }

{-------------------------------------------------------------------------------
  Beamable instance
-------------------------------------------------------------------------------}

instance KnownFields r => Beamable (AnonTable r) where
  zipBeamFieldsM ::
       forall f g h m.
       Applicative m
    => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
    -> AnonTable r f
    -> AnonTable r g
    -> m (AnonTable r h)
  zipBeamFieldsM combine (AnonTable x) (AnonTable y) =
      AnonTable <$>
        Anon.zipWithM
          (\fx gy ->
               unwrapColumnar' <$>
                 combine (wrapColumnar' fx) (wrapColumnar' gy))
          x
          y

  tblSkeleton :: TableSkeleton (AnonTable r)
  tblSkeleton = AnonTable $ Anon.pure Ignored

wrapColumnar' :: f a -> Columnar' f a
wrapColumnar' = unsafeCoerce

unwrapColumnar' :: Columnar' h a -> h a
unwrapColumnar' = unsafeCoerce

{-------------------------------------------------------------------------------
  GHC.Generic Interop for Beam
  
  Beam heavily relies on GHC.Generics for things like 'insertValues'.
  We provide a trivial GHC.Generic instance using a custom representation
  type, and we provide beam's generic typeclass instances for this rep.
-------------------------------------------------------------------------------}

newtype AnonTableRep (r :: Row Type) (f :: Type -> Type) x = AnonTableRep (AnonTable r f)

instance KnownFields r => GHC.Generic (AnonTable r f) where
  type Rep (AnonTable r f) = AnonTableRep r f
  from = AnonTableRep
  to (AnonTableRep tbl) = tbl
