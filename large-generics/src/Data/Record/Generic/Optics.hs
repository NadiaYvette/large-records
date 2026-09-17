{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Profunctor optics for large records using 'optics-core'
module Data.Record.Generic.Optics (
    -- * General optics for generic records
    genericIso
  , genericLens
  , RepOptic(..)
  , repOptic
  , repOptics
    -- * Optics for simple records
  , SimpleRecordOptic(..)
  , opticsForSimpleRecord
    -- * Optics for higher-kinded records
  , HKRecordOptic(..)
  , opticsForHKRecord
    -- * Optics for regular records
  , RegularRecordOptic(..)
  , opticsForRegularRecord
    -- * Normal form and interpretation optics
  , normalForm1Iso
  , interpretedIso
  , standardInterpretationIso
    -- * Conversions with van Laarhoven lenses
  , vlToLens
  , lensToVL
  ) where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind
import Optics.Core hiding (to)

import Data.Record.Generic
import Data.Record.Generic.Lens.VL (RegularField(..), IsRegularField(..))
import Data.Record.Generic.Transform

import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  General optics
-------------------------------------------------------------------------------}

-- | Isomorphism between a record and its generic representation
genericIso :: Generic a => Iso' a (Rep I a)
genericIso = iso from to

-- | Lens between a record and its generic representation
genericLens :: Generic a => Lens' a (Rep I a)
genericLens = lens from (\_ r -> to r)

-- | Lens into an index of 'Rep'
data RepOptic f a x where
  RepOptic :: Lens' (Rep f a) (f x) -> RepOptic f a x

repOptic :: Rep.Index a x -> Lens' (Rep f a) (f x)
repOptic idx = lens (Rep.getAtIndex idx) (\r x -> Rep.putAtIndex idx x r)

-- | Construct optics for each field in 'Rep'
repOptics :: forall a f. Generic a => Rep (RepOptic f a) a
repOptics = Rep.map aux Rep.allIndices
  where
    aux :: Rep.Index a x -> RepOptic f a x
    aux idx = RepOptic (repOptic idx)

{-------------------------------------------------------------------------------
  Simple records
-------------------------------------------------------------------------------}

data SimpleRecordOptic a b where
  SimpleRecordOptic :: Lens' a b -> SimpleRecordOptic a b

-- | Construct optics for each field in a simple record
opticsForSimpleRecord :: forall a. Generic a => Rep (SimpleRecordOptic a) a
opticsForSimpleRecord =
    Rep.map (\(RepOptic l) -> SimpleRecordOptic (genericLens % l % iIso)) repOptics
  where
    iIso :: Iso' (I x) x
    iIso = iso unI I

{-------------------------------------------------------------------------------
  Higher-kinded records
-------------------------------------------------------------------------------}

data HKRecordOptic d (f :: Type -> Type) tbl x where
  HKRecordOptic :: Lens' (tbl f) (Interpret (d f) x) -> HKRecordOptic d f tbl x

-- | Optics for higher-kinded records
opticsForHKRecord ::
     forall d tbl f.
     ( Generic (tbl f)
     , Generic (tbl Uninterpreted)
     , HasNormalForm (d f) (tbl f) (tbl Uninterpreted)
     )
  => Proxy d -> Rep (HKRecordOptic d f tbl) (tbl Uninterpreted)
opticsForHKRecord d = Rep.map aux fromRepOptics
  where
    fromRepOptics :: Rep (RepOptic (Interpret (d f)) (tbl Uninterpreted)) (tbl Uninterpreted)
    fromRepOptics = repOptics

    aux :: forall x.
         RepOptic (Interpret (d f)) (tbl Uninterpreted) x
      -> HKRecordOptic d f tbl x
    aux (RepOptic l) = HKRecordOptic $
          genericLens
        % normalForm1Iso d
        % l

{-------------------------------------------------------------------------------
  Regular records
-------------------------------------------------------------------------------}

data RegularRecordOptic tbl f x where
  RegularRecordOptic :: Lens' (tbl f) (f x) -> RegularRecordOptic tbl f x

-- | Optics into higher-kinded records with regular fields
opticsForRegularRecord ::
     forall d tbl f.
     ( Generic (tbl (RegularRecordOptic tbl f))
     , Generic (tbl Uninterpreted)
     , Generic (tbl f)
     , HasNormalForm (d (RegularRecordOptic tbl f)) (tbl (RegularRecordOptic tbl f)) (tbl Uninterpreted)
     , HasNormalForm (d f) (tbl f) (tbl Uninterpreted)
     , Constraints (tbl Uninterpreted) (IsRegularField Uninterpreted)
     , StandardInterpretation d (RegularRecordOptic tbl f)
     , StandardInterpretation d f
     )
  => Proxy d -> tbl (RegularRecordOptic tbl f)
opticsForRegularRecord d = to . denormalize1 d $
    Rep.cmap
      (Proxy @(IsRegularField Uninterpreted))
      aux
      (opticsForHKRecord d)
  where
    aux :: forall x.
         IsRegularField Uninterpreted x
      => HKRecordOptic d f tbl x
      -> Interpret (d (RegularRecordOptic tbl f)) x
    aux (HKRecordOptic l) =
        case isRegularField (Proxy @(Uninterpreted x)) of
          RegularField -> toStandardInterpretation d $ RegularRecordOptic $
             l % standardInterpretationIso d

{-------------------------------------------------------------------------------
  Normal form & interpretation isomorphisms
-------------------------------------------------------------------------------}

normalForm1Iso ::
     HasNormalForm (d f) (x f) (x Uninterpreted)
  => Proxy d
  -> Iso' (Rep I (x f)) (Rep (Interpret (d f)) (x Uninterpreted))
normalForm1Iso p = iso (normalize1 p) (denormalize1 p)

interpretedIso :: Iso' (Interpret d x) (Interpreted d x)
interpretedIso = iso (\(Interpret x) -> x) Interpret

standardInterpretationIso ::
     forall d f x.
     StandardInterpretation d f
  => Proxy d
  -> Iso' (Interpret (d f) (Uninterpreted x)) (f x)
standardInterpretationIso p =
    iso (fromStandardInterpretation p) (toStandardInterpretation p)

{-------------------------------------------------------------------------------
  Conversions between van Laarhoven and Optics
-------------------------------------------------------------------------------}

-- | Convert a van Laarhoven lens to an 'Optics.Lens'
vlToLens :: (forall f. Functor f => (a -> f a) -> s -> f s) -> Lens' s a
vlToLens l = lens getter setter
  where
    getter s = getConst (l Const s)
    setter s a = runIdentity (l (\_ -> Identity a) s)

-- | Convert an 'Optics.Lens' to a van Laarhoven lens
lensToVL :: Lens' s a -> (forall f. Functor f => (a -> f a) -> s -> f s)
lensToVL l f s = (\a' -> set l a' s) <$> f (view l s)
