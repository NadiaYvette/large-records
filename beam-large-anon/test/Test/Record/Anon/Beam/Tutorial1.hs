{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Beam anonymous record tutorial — replicate the beam tutorial using
-- large-anon anonymous records.
module Test.Record.Anon.Beam.Tutorial1 (tests) where

import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Text (Text)
import Database.Beam hiding (Generic)
import Database.Beam.Schema.Tables
import Database.Beam.Sqlite
import Optics.Core ((^.))

import qualified Data.List.NonEmpty     as NE
import qualified GHC.Generics           as GHC
import qualified Database.SQLite.Simple as SQLite
import Data.String (fromString)
import qualified Unsafe.Coerce          as Unsafe

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon
import Data.Record.Anon.Beam
import qualified Data.Record.Anon.Advanced as Anon

import Test.Record.Anon.Beam.Util.SQLite

{-------------------------------------------------------------------------------
  User table defined as an AnonTable
-------------------------------------------------------------------------------}

-- | Row type for the user table.
type UserRow = '[ "userEmail"     ':= Text
                , "userFirstName" ':= Text
                , "userLastName"  ':= Text
                , "userPassword"  ':= Text
                ]

-- | The user table type.
type UserT = AnonTable UserRow

-- | Convenience alias for a fully-instantiated user row.
type User   = UserT Identity
type UserId = PrimaryKey UserT Identity

-- | Provide the 'Table' instance: we use the email as the primary key.
instance Table UserT where
  newtype PrimaryKey UserT f = UserId (Columnar f Text)
    deriving stock (GHC.Generic)
    deriving anyclass (Beamable)

  primaryKey tbl = UserId (Unsafe.unsafeCoerce (Anon.get #userEmail (unAnonTable tbl)))

deriving instance Show (Columnar f Text) => Show (PrimaryKey UserT f)
deriving instance Eq   (Columnar f Text) => Eq   (PrimaryKey UserT f)

instance Show (UserT Identity) where
  show (AnonTable r) = show r

instance Eq (UserT Identity) where
  AnonTable r1 == AnonTable r2 = r1 == r2

-- | Construct users using overloaded labels and the anonymous record API.
mkUser :: Text -> Text -> Text -> Text -> User
mkUser email first last pass =
    AnonTable $
      Anon.insert #userEmail     (Identity email) $
      Anon.insert #userFirstName (Identity first) $
      Anon.insert #userLastName  (Identity last)  $
      Anon.insert #userPassword  (Identity pass)  $
      Anon.empty

james, betty, sam :: User
james = mkUser "james@example.com" "James" "Smith"  "b4cc344d25a2efe540adbf2678e2304c"
betty = mkUser "betty@example.com" "Betty" "Jones"  "82b054bd83ffad9b6cf8bdb98ce3cc2f"
sam   = mkUser "sam@example.com"   "Sam"   "Taylor" "332532dcfaa1cbf61e2a266bd723612c"

{-------------------------------------------------------------------------------
  Shopping cart database
-------------------------------------------------------------------------------}

type ShoppingCartRow = '[ "shoppingCartUsers" ':= TableEntity UserT ]

type ShoppingCartDb f = AnonDb ShoppingCartRow f

-- | We implement 'zipTables' using 'anonZipTables'.
instance AllFields ShoppingCartRow (AnonDbEntity be)
      => Database be (AnonDb ShoppingCartRow)

shoppingCartDb :: DatabaseSettings be (AnonDb ShoppingCartRow)
shoppingCartDb = defaultDbSettings

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Anon.Beam.Tutorial1" [
      testCase "tblSkeleton"       test_tblSkeleton
    , testCase "defTblSettings"    test_defTblSettings
    , testCase "insertSelect"      test_insertSelect
    ]

test_tblSkeleton :: Assertion
test_tblSkeleton = do
    let skel = tblSkeleton :: TableSkeleton UserT
    -- Check it's an AnonTable wrapping a record of Ignored values
    let AnonTable rec = skel
    -- We just check the shape: the record should have 4 fields, all Ignored
    let names = Anon.collapse (Anon.reifyKnownFields (Proxy @UserRow))
    assertEqual "number of fields" 4 (length names)

test_defTblSettings :: Assertion
test_defTblSettings = do
    let settings = anonDefTblFieldSettings :: UserT (TableField UserT)
    let AnonTable rec = settings
    -- Check that the email field has the right column name
    let emailField = Anon.get #userEmail rec
    assertEqual "email column name" "user_email" (_fieldName emailField)
    let firstField = Anon.get #userFirstName rec
    assertEqual "first_name column name" "user_first_name" (_fieldName firstField)
    let lastField = Anon.get #userLastName rec
    assertEqual "last_name column name" "user_last_name" (_fieldName lastField)
    let passField = Anon.get #userPassword rec
    assertEqual "password column name" "user_password" (_fieldName passField)

test_insertSelect :: Assertion
test_insertSelect = runInMemory $ \conn -> do
    liftIO $ runSQLite conn $
      "CREATE TABLE cart_users (user_email VARCHAR NOT NULL, user_first_name VARCHAR NOT NULL, user_last_name VARCHAR NOT NULL, user_password VARCHAR NOT NULL, PRIMARY KEY( user_email ));"

    let usersTable = Anon.get #shoppingCartUsers (unAnonDb shoppingCartDb)
    runInsert $ insert usersTable $ insertValues [james, betty, sam]

    let allUsers = all_ usersTable
    users <- runSelectReturningList $ select allUsers
    liftIO $ assertEqual "users" [james, betty, sam] users

-- | Helper to run raw SQL via sqlite-simple
runSQLite :: SQLite.Connection -> String -> IO ()
runSQLite conn sql = SQLite.execute_ conn (fromString sql)
