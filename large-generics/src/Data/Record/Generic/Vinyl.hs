{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Interoperability between large records and 'vinyl' extensible records
module Data.Record.Generic.Vinyl (
    -- * Conversion functions
    toVinylGeneric
  , fromVinylGeneric
    -- * Type classes for conversion
  , ToVinyl(..)
  , FromVinyl(..)
  ) where

import Data.Kind
import Data.Primitive.SmallArray
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Functor (ElField(..))
import GHC.Exts (Any)
import GHC.TypeLits (Symbol)

import Data.Record.Generic
import Data.Record.Generic.Rep.Internal (noInlineUnsafeCo)
import qualified Data.Record.Generic.Rep as Rep

-- | Convert list of untyped values to a Vinyl 'Rec ElField'
class ToVinyl (fs :: [(Symbol, Type)]) where
  toVinylRec :: [Any] -> Rec ElField fs

instance ToVinyl '[] where
  toVinylRec _ = RNil

instance ToVinyl fs => ToVinyl ('(s, t) : fs) where
  toVinylRec [] = error "toVinylRec: unexpected empty list"
  toVinylRec (x : xs) = Field (noInlineUnsafeCo x) :& toVinylRec xs

-- | Convert a Vinyl 'Rec ElField' to a list of untyped values
class FromVinyl (fs :: [(Symbol, Type)]) where
  fromVinylRec :: Rec ElField fs -> [Any]

instance FromVinyl '[] where
  fromVinylRec RNil = []

instance FromVinyl fs => FromVinyl ('(s, t) : fs) where
  fromVinylRec (Field x :& xs) = noInlineUnsafeCo x : fromVinylRec xs

-- | Convert any record with a 'Generic' instance to a Vinyl 'Rec ElField'
toVinylGeneric :: forall a. (Generic a, ToVinyl (MetadataOf a)) => a -> Rec ElField (MetadataOf a)
toVinylGeneric a =
    toVinylRec (Rep.collapse (Rep.map (\(I x) -> K (noInlineUnsafeCo x)) (from a)))

-- | Construct any record with a 'Generic' instance from a Vinyl 'Rec ElField'
fromVinylGeneric :: forall a. (Generic a, FromVinyl (MetadataOf a)) => Rec ElField (MetadataOf a) -> a
fromVinylGeneric r =
    to (Rep (smallArrayFromList (map I (fromVinylRec r))))
