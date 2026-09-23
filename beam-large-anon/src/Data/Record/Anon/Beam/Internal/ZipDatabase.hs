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

-- | 'Database' instance support for anonymous record databases.
module Data.Record.Anon.Beam.Internal.ZipDatabase (
    anonZipTables
  ) where

import Data.Proxy
import Database.Beam.Schema.Tables

import Data.Record.Anon
import qualified Data.Record.Anon.Advanced as Anon

import Data.Record.Anon.Beam.Internal.DbSettings

{-------------------------------------------------------------------------------
  GHC.Generic interop (GZipDatabase)
-------------------------------------------------------------------------------}

instance ( AllFields r (AnonDbEntity be)
         )
      => GZipDatabase be f g h (AnonDbRep r f) (AnonDbRep r g) (AnonDbRep r h) where
  gZipDatabase _pbe combine (AnonDbRep (AnonDb x)) (AnonDbRep (AnonDb y)) =
      AnonDbRep . AnonDb <$>
        Anon.czipWithM
          (Proxy @(AnonDbEntity be))
          combine
          x
          y

{-------------------------------------------------------------------------------
  Legacy direct zipTables
-------------------------------------------------------------------------------}

anonZipTables ::
     forall be r f g h m.
     ( AllFields r (AnonDbEntity be)
     , Applicative m
     )
  => Proxy be
  -> (forall tbl.
          ( IsDatabaseEntity be tbl
          , DatabaseEntityRegularRequirements be tbl
          )
       => f tbl -> g tbl -> m (h tbl))
  -> AnonDb r f
  -> AnonDb r g
  -> m (AnonDb r h)
anonZipTables _pbe combine (AnonDb x) (AnonDb y) =
    AnonDb <$>
      Anon.czipWithM
        (Proxy @(AnonDbEntity be))
        combine
        x
        y
