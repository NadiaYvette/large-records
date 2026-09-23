{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Default table and database settings for anonymous records.
module Data.Record.Anon.Beam.Internal.DbSettings (
    -- * Table settings
    anonDefTblFieldSettings
    -- * Database wrapper
  , AnonDb(..)
  , AnonDbRep(..)
  , AnonDbEntity
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Kind
import Data.Proxy
import Data.Text (Text)

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import Database.Beam.Schema.Tables

import qualified GHC.Generics           as GHC
import qualified Data.Text              as T
import qualified Data.Record.Anon.Advanced as Anon

import Data.Record.Anon.Beam.Internal.ZipTables

{-------------------------------------------------------------------------------
  Table settings
-------------------------------------------------------------------------------}

anonDefTblFieldSettings ::
     forall r. KnownFields r
  => AnonTable r (TableField (AnonTable r))
anonDefTblFieldSettings =
    AnonTable $
      Anon.map
        (\(K fieldStr) ->
           let nm = T.pack fieldStr
           in  TableField (nm :| []) (beamUnCamelCase nm))
        (Anon.reifyKnownFields (Proxy @r))

beamUnCamelCase :: Text -> Text
beamUnCamelCase t =
    T.toLower $ T.intercalate "_" $ go t
  where
    go s
      | T.null s  = []
      | otherwise =
          let (lower, rest) = T.break isUpper s
          in  if T.null lower
              then case T.uncons rest of
                     Nothing       -> []
                     Just (c, cs)  -> go (T.cons (toLower c) cs)
              else lower : go rest

    isUpper c = c >= 'A' && c <= 'Z'
    toLower c
      | isUpper c = toEnum (fromEnum c + 32)
      | otherwise = c

{-------------------------------------------------------------------------------
  GHC.Generic interop (GDefaultTableFieldSettings)
-------------------------------------------------------------------------------}

instance KnownFields r => GDefaultTableFieldSettings (AnonTableRep r (TableField (AnonTable r)) ()) where
  gDefTblFieldSettings _ = AnonTableRep (anonDefTblFieldSettings @r)

{-------------------------------------------------------------------------------
  Database wrapper
-------------------------------------------------------------------------------}

newtype AnonDb (r :: Row Type) (f :: Type -> Type) =
    AnonDb { unAnonDb :: Record f r }

class (IsDatabaseEntity be tbl, DatabaseEntityRegularRequirements be tbl)
    => AnonDbEntity be tbl
instance (IsDatabaseEntity be tbl, DatabaseEntityRegularRequirements be tbl)
    => AnonDbEntity be tbl

{-------------------------------------------------------------------------------
  GHC.Generic interop (GAutoDbSettings)
-------------------------------------------------------------------------------}

newtype AnonDbRep (r :: Row Type) (f :: Type -> Type) x = AnonDbRep (AnonDb r f)

instance KnownFields r => GHC.Generic (AnonDb r f) where
  type Rep (AnonDb r f) = AnonDbRep r f
  from = AnonDbRep
  to (AnonDbRep db) = db

instance ( KnownFields r
         , AllFields r (AnonDbEntityDefault be)
         )
      => GAutoDbSettings (AnonDbRep r (DatabaseEntity be (AnonDb r)) ()) where
  autoDbSettings' =
      AnonDbRep $ AnonDb $
        Anon.cmap
          (Proxy @(AnonDbEntityDefault be))
          (\(K fieldStr) -> DatabaseEntity (dbEntityAuto (T.pack fieldStr)))
          (Anon.reifyKnownFields (Proxy @r))

class (IsDatabaseEntity be tbl, DatabaseEntityDefaultRequirements be tbl)
    => AnonDbEntityDefault be tbl
instance (IsDatabaseEntity be tbl, DatabaseEntityDefaultRequirements be tbl)
    => AnonDbEntityDefault be tbl
