{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Tests for 'zipBeamFieldsM' on anonymous tables.
module Test.Record.Anon.Beam.Zipping (tests) where

import Data.Functor.Identity (Identity(..))
import Database.Beam hiding (Generic)
import Database.Beam.Schema.Tables
import Test.Tasty
import Test.Tasty.HUnit
import qualified Unsafe.Coerce as Unsafe

import qualified GHC.Generics as GHC

import Data.Record.Anon
import Data.Record.Anon.Beam
import qualified Data.Record.Anon.Advanced as Anon

{-------------------------------------------------------------------------------
  A simple two-field anonymous table
-------------------------------------------------------------------------------}

type ThingRow = '[ "thingKey" ':= Int, "thingFlag" ':= Bool ]
type ThingT   = AnonTable ThingRow

instance Table ThingT where
  newtype PrimaryKey ThingT f = ThingKey (Columnar f Int)
    deriving stock (GHC.Generic)
    deriving anyclass (Beamable)

  primaryKey tbl = ThingKey (Unsafe.unsafeCoerce (Anon.get #thingKey (unAnonTable tbl)))

deriving instance Show (Columnar f Int)  => Show (PrimaryKey ThingT f)
deriving instance Eq   (Columnar f Int)  => Eq   (PrimaryKey ThingT f)

instance Show (ThingT Identity) where
  show (AnonTable r) = show r

instance Eq (ThingT Identity) where
  AnonTable r1 == AnonTable r2 = r1 == r2

-- | A newtype for endomorphisms, used to test 'zipBeamFieldsM'.
newtype EndoFn a = EndoFn (a -> a)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Anon.Beam.Zipping" [
      testCase "zipBeamFields" test_zipBeamFields
    , testCase "tblSkeleton"   test_tblSkeleton
    ]

test_zipBeamFields :: Assertion
test_zipBeamFields = do
    let apply :: forall a.
                 Columnar' EndoFn a
              -> Columnar' Identity a
              -> Identity (Columnar' Identity a)
        apply (Columnar' (EndoFn f)) (Columnar' x) = Identity (Columnar' (f x))

    let fnTbl :: ThingT EndoFn
        fnTbl = AnonTable $
                  Anon.insert #thingKey  (EndoFn succ) $
                  Anon.insert #thingFlag (EndoFn not)  $
                  Anon.empty

    let argTbl :: ThingT Identity
        argTbl = AnonTable $
                   Anon.insert #thingKey  (Identity 41)   $
                   Anon.insert #thingFlag (Identity True)  $
                   Anon.empty

    let expected :: ThingT Identity
        expected = AnonTable $
                     Anon.insert #thingKey  (Identity 42)    $
                     Anon.insert #thingFlag (Identity False)  $
                     Anon.empty

    let result = runIdentity (zipBeamFieldsM apply fnTbl argTbl)
    assertEqual "" expected result

test_tblSkeleton :: Assertion
test_tblSkeleton = do
    -- Just check it doesn't throw; shape is correct by construction.
    let skel = tblSkeleton :: TableSkeleton ThingT
    -- Values needed should equal number of fields
    let n = tableValuesNeeded (Proxy @ThingT)
    assertEqual "values needed" 2 n
