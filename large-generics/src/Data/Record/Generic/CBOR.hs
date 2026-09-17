{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | CBOR serialization infrastructure for large records
module Data.Record.Generic.CBOR (
    -- * Configuration
    CBORFormat(..)
  , CBOROptions(..)
  , defaultCBOROptions
    -- * Low-level record codecs
  , encodeRecordArray
  , decodeRecordArray
  , encodeRecordMap
  , decodeRecordMap
    -- * Generic functions using 'Serialise'
  , gencodeCBOR
  , gdecodeCBOR
  , gencodeCBORWith
  , gdecodeCBORWith
  ) where

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Codec.CBOR.Term (decodeTerm)
import Codec.Serialise.Class (Serialise(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Primitive.SmallArray
import Data.Proxy
import qualified Data.Text as T
import GHC.Exts (Any)

import Data.Record.Generic
import Data.Record.Generic.Rep.Internal (noInlineUnsafeCo)
import qualified Data.Record.Generic.Rep as Rep

-- | CBOR encoding representation
data CBORFormat =
    -- | Encode record as a fixed-length CBOR array of its fields
    CBORArray
    -- | Encode record as a CBOR map with field names as keys
  | CBORMap
  deriving (Show, Eq)

-- | Options for CBOR serialization
data CBOROptions = CBOROptions {
      cborFormat             :: CBORFormat
    , cborFieldLabelModifier :: String -> String
    }

defaultCBOROptions :: CBOROptions
defaultCBOROptions = CBOROptions {
      cborFormat             = CBORArray
    , cborFieldLabelModifier = id
    }

{-------------------------------------------------------------------------------
  Low-level record codecs
-------------------------------------------------------------------------------}

-- | Encode record fields as a CBOR array
encodeRecordArray ::
     forall a f.
     Generic a
  => (forall x. f x -> Encoding)
  -> Rep f a
  -> Encoding
encodeRecordArray encodeField rep =
    encodeListLen (fromIntegral (recordSize md))
      <> mconcat (Rep.collapse (Rep.map (K . encodeField) rep))
  where
    md = metadata (Proxy @a)

-- | Decode record fields from a CBOR array
decodeRecordArray ::
     forall s a.
     Generic a
  => Rep (Decoder s) a
  -> Decoder s (Rep I a)
decodeRecordArray decoders = do
    len <- decodeListLen
    let expected = recordSize (metadata (Proxy @a))
    if len /= expected
      then fail $ "decodeRecordArray: expected array of length " ++ show expected ++ ", got " ++ show len
      else Rep.sequenceA (Rep.map (\dec -> Comp (I <$> dec)) decoders)

-- | Encode record fields as a CBOR map
encodeRecordMap ::
     forall a f.
     Generic a
  => (String -> String)
  -> (forall x. f x -> Encoding)
  -> Metadata a
  -> Rep f a
  -> Encoding
encodeRecordMap labelMod encodeField md rep =
    encodeMapLen (fromIntegral (recordSize md))
      <> mconcat (Rep.collapse pairs)
  where
    pairs = Rep.zipWith
              (\(K name) fx -> K (encodeString (T.pack (labelMod name)) <> encodeField fx))
              (recordFieldNames md)
              rep

-- | Decode record fields from a CBOR map (handles unordered keys and skips unknown keys)
decodeRecordMap ::
     forall s a.
     (String -> String)
  -> Metadata a
  -> Rep (Decoder s) a
  -> Decoder s (Rep I a)
decodeRecordMap labelMod md (Rep decArray) = do
    mapLen <- decodeMapLen
    values <- readPairs mapLen Map.empty
    -- Build output array
    let total = recordSize md
    resultArr <- buildResult 0 total values
    return (Rep resultArr)
  where
    -- Map from field label (modified) to field index
    fieldIndexMap :: Map T.Text Int
    fieldIndexMap =
        Map.fromList
          [ (T.pack (labelMod name), i)
          | (i, name) <- zip [0..] (Rep.collapse (recordFieldNames md))
          ]

    fieldNamesArr :: [String]
    fieldNamesArr = Rep.collapse (recordFieldNames md)

    readPairs :: Int -> Map Int Any -> Decoder s (Map Int Any)
    readPairs 0 acc = return acc
    readPairs n acc = do
        k <- decodeString
        case Map.lookup k fieldIndexMap of
          Just idx -> do
            let dec = indexSmallArray decArray idx
            val <- dec
            readPairs (n - 1) (Map.insert idx (noInlineUnsafeCo val) acc)
          Nothing -> do
            -- Unknown field: skip it cleanly using decodeTerm
            _ <- decodeTerm
            readPairs (n - 1) acc

    buildResult :: Int -> Int -> Map Int Any -> Decoder s (SmallArray (I Any))
    buildResult cur maxIdx valMap
      | cur == maxIdx = do
          let elems = [ case Map.lookup i valMap of
                          Just v  -> I v
                          Nothing -> error "unreachable"
                      | i <- [0 .. maxIdx - 1]
                      ]
          return $ smallArrayFromList elems
      | otherwise =
          case Map.lookup cur valMap of
            Just _  -> buildResult (cur + 1) maxIdx valMap
            Nothing ->
              let fldName = if cur < length fieldNamesArr then fieldNamesArr !! cur else show cur
              in fail $ "decodeRecordMap: missing field " ++ show fldName

{-------------------------------------------------------------------------------
  Generic functions using 'Serialise'
-------------------------------------------------------------------------------}

gencodeCBOR :: forall a. (Generic a, Constraints a Serialise) => a -> Encoding
gencodeCBOR = gencodeCBORWith defaultCBOROptions

gdecodeCBOR :: forall s a. (Generic a, Constraints a Serialise) => Decoder s a
gdecodeCBOR = gdecodeCBORWith defaultCBOROptions

gencodeCBORWith ::
     forall a.
     (Generic a, Constraints a Serialise)
  => CBOROptions
  -> a
  -> Encoding
gencodeCBORWith CBOROptions{..} a =
    case cborFormat of
      CBORArray -> encodeRecordArray (\(K enc) -> enc) encodings
      CBORMap   -> encodeRecordMap cborFieldLabelModifier (\(K enc) -> enc) md encodings
  where
    md = metadata (Proxy @a)
    encodings :: Rep (K Encoding) a
    encodings = Rep.cmap (Proxy @Serialise) (\(I x) -> K (encode x)) (from a)

gdecodeCBORWith ::
     forall s a.
     (Generic a, Constraints a Serialise)
  => CBOROptions
  -> Decoder s a
gdecodeCBORWith CBOROptions{..} =
    case cborFormat of
      CBORArray -> to <$> decodeRecordArray decoders
      CBORMap   -> to <$> decodeRecordMap cborFieldLabelModifier md decoders
  where
    md = metadata (Proxy @a)
    decoders :: Rep (Decoder s) a
    decoders = Rep.cmap (Proxy @Serialise) (\(K _) -> decode) (recordFieldNames md)
