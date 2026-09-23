-- | Integration of @large-anon@ anonymous records with @beam-core@.
--
-- This module provides everything needed to use 'Record f r' anonymous records
-- (from @large-anon@) as beam database tables.
--
-- = Quick start
--
-- Enable the @Data.Record.Anon.Plugin@ GHC plugin in your module, then:
--
-- @
-- {-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}
--
-- import Data.Record.Anon
-- import Data.Record.Anon.Beam
-- import qualified Data.Record.Anon.Advanced as Anon
--
-- -- Define the table type (note: Columnar is not needed for AnonTable fields)
-- type UserRow = AnonTable '[ "email" ':= Text, "name" ':= Text ]
--
-- -- Define the primary key
-- instance KnownFields '[ "email" ':= Text, "name" ':= Text ]
--       => Table UserRow where
--   newtype PrimaryKey UserRow f = UserId (Columnar f Text)
--     deriving stock GHC.Generic
--     deriving anyclass Beamable
--   primaryKey tbl = UserId (Anon.get #email (unAnonTable tbl))
--
-- deriving instance Show (Columnar f Text) => Show (PrimaryKey UserRow f)
-- deriving instance Eq   (Columnar f Text) => Eq   (PrimaryKey UserRow f)
--
-- -- Table settings
-- userSettings :: UserRow (TableField UserRow)
-- userSettings = anonDefTblFieldSettings
-- @
--
-- = Databases
--
-- To use an anonymous record as a beam /database/:
--
-- @
-- type MyDb f = AnonDb '[ "users" ':= TableEntity UserRow ] f
--
-- instance AllFields '[ "users" ':= TableEntity UserRow ] (AnonDbEntity MyBackend)
--       => Database MyBackend (AnonDb '[ "users" ':= TableEntity UserRow ])
--   where
--     zipTables = anonZipTables
--
-- myDb :: DatabaseSettings MyBackend (AnonDb '[ "users" ':= TableEntity UserRow ])
-- myDb = anonAutoDbSettings
-- @
module Data.Record.Anon.Beam (
    -- * Table wrapper
    AnonTable(..)
    -- * Beamable (automatically derived for KnownFields r)
    -- * Table settings
  , anonDefTblFieldSettings
    -- * Database wrapper
  , AnonDb(..)
  , anonZipTables
    -- * Constraint utilities
  , anonWithConstraints
  , anonWithConstrainedFields
    -- * Re-exported constraint synonyms
  , AnonDbEntity
  , AnonFromBackendRow
  ) where

import Data.Record.Anon.Beam.Internal.Constraints
import Data.Record.Anon.Beam.Internal.DbSettings
import Data.Record.Anon.Beam.Internal.FromBackendRow
import Data.Record.Anon.Beam.Internal.ZipDatabase
import Data.Record.Anon.Beam.Internal.ZipTables
