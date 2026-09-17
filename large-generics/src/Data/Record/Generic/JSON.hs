{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Generic conversion to/from JSON
module Data.Record.Generic.JSON (
    -- * Standard generic functions
    gtoJSON
  , gparseJSON
  , gtoEncoding
  , gtoPairs
    -- * Configuration options
  , JSONOptions(..)
  , defaultJSONOptions
    -- * Configurable generic functions
  , gtoJSONWith
  , gparseJSONWith
  , gtoEncodingWith
  , gtoPairsWith
  ) where

import Data.Aeson
import Data.Aeson.Encoding (pair)
import Data.Aeson.Types
import Data.Proxy
import Data.String

import Data.Record.Generic
import qualified Data.Record.Generic.Rep as Rep

-- | Options for customizing JSON encoding and decoding
data JSONOptions = JSONOptions {
      jsonFieldLabelModifier :: String -> String
    , jsonOmitNothingFields  :: Bool
    }

defaultJSONOptions :: JSONOptions
defaultJSONOptions = JSONOptions {
      jsonFieldLabelModifier = id
    , jsonOmitNothingFields  = False
    }

gtoJSON :: forall a. (Generic a, Constraints a ToJSON) => a -> Value
gtoJSON = gtoJSONWith defaultJSONOptions

gtoJSONWith :: forall a. (Generic a, Constraints a ToJSON) => JSONOptions -> a -> Value
gtoJSONWith opts =
      object
    . filterOmit
    . Rep.collapse
    . Rep.zipWith (mapKKK $ \n x -> (fromString (jsonFieldLabelModifier opts n), x)) (recordFieldNames md)
    . Rep.cmap (Proxy @ToJSON) (K . toJSON . unI)
    . from
  where
    md = metadata (Proxy @a)

    filterOmit
      | jsonOmitNothingFields opts = filter (\(_, v) -> v /= Null)
      | otherwise                  = id

gtoEncoding :: forall a. (Generic a, Constraints a ToJSON) => a -> Encoding
gtoEncoding = gtoEncodingWith defaultJSONOptions

gtoEncodingWith :: forall a. (Generic a, Constraints a ToJSON) => JSONOptions -> a -> Encoding
gtoEncodingWith opts =
      pairs
    . mconcat
    . filterOmit
    . Rep.collapse
    . Rep.zipWith (mapKKK $ \n (v, enc) -> (v, pair (fromString (jsonFieldLabelModifier opts n)) enc)) (recordFieldNames md)
    . Rep.cmap (Proxy @ToJSON) (\(I x) -> K (toJSON x, toEncoding x))
    . from
  where
    md = metadata (Proxy @a)

    filterOmit
      | jsonOmitNothingFields opts = map snd . filter (\(v, _) -> v /= Null)
      | otherwise                  = map snd

gtoPairs :: forall a. (Generic a, Constraints a ToJSON) => a -> [Pair]
gtoPairs = gtoPairsWith defaultJSONOptions

gtoPairsWith :: forall a. (Generic a, Constraints a ToJSON) => JSONOptions -> a -> [Pair]
gtoPairsWith opts =
      filterOmit
    . Rep.collapse
    . Rep.zipWith (mapKKK $ \n x -> (fromString (jsonFieldLabelModifier opts n), x)) (recordFieldNames md)
    . Rep.cmap (Proxy @ToJSON) (K . toJSON . unI)
    . from
  where
    md = metadata (Proxy @a)

    filterOmit
      | jsonOmitNothingFields opts = filter (\(_, v) -> v /= Null)
      | otherwise                  = id

gparseJSON :: forall a. (Generic a, Constraints a FromJSON) => Value -> Parser a
gparseJSON = gparseJSONWith defaultJSONOptions

gparseJSONWith :: forall a. (Generic a, Constraints a FromJSON) => JSONOptions -> Value -> Parser a
gparseJSONWith opts =
    withObject (recordName md) (fmap to . Rep.sequenceA . aux)
  where
    md = metadata (Proxy @a)

    aux :: Object -> Rep (Parser :.: I) a
    aux obj =
        Rep.cmap
          (Proxy @FromJSON)
          (\(K fld) -> Comp (I <$> getField (jsonFieldLabelModifier opts fld)))
          (recordFieldNames md)
      where
        getField :: forall x. FromJSON x => String -> Parser x
        getField fld =
          if jsonOmitNothingFields opts
            then do
              mVal <- obj .:? fromString fld
              case mVal of
                Just v  -> return v
                Nothing -> parseJSON Null
            else obj .: fromString fld
