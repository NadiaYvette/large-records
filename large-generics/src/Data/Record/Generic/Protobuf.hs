{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Protocol Buffers binary wire-format serialization for large records
module Data.Record.Generic.Protobuf (
    -- * Wire Types
    WireType(..)
  , wireTypeToWord
  , wordToWireType
    -- * Field class
  , ProtobufField(..)
    -- * Varint & Wire primitives
  , encodeVarint
  , decodeVarint
  , encodeZigZag32
  , decodeZigZag32
  , encodeZigZag64
  , decodeZigZag64
    -- * Generic Protobuf serialization
  , gencodeProtobuf
  , gdecodeProtobuf
  , gencodeProtobufWith
  , gdecodeProtobufWith
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Product (Product(Pair))
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word

import Data.Record.Generic
import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  Wire Types
-------------------------------------------------------------------------------}

data WireType =
    WireVarint          -- ^ 0: int32, int64, uint32, bool, enum
  | WireFixed64         -- ^ 1: fixed64, double
  | WireLengthDelimited -- ^ 2: string, bytes, embedded messages
  | WireFixed32         -- ^ 5: fixed32, float
  deriving (Show, Eq)

wireTypeToWord :: WireType -> Word32
wireTypeToWord WireVarint          = 0
wireTypeToWord WireFixed64         = 1
wireTypeToWord WireLengthDelimited = 2
wireTypeToWord WireFixed32         = 5

wordToWireType :: Word32 -> Either String WireType
wordToWireType 0 = Right WireVarint
wordToWireType 1 = Right WireFixed64
wordToWireType 2 = Right WireLengthDelimited
wordToWireType 5 = Right WireFixed32
wordToWireType w = Left $ "wordToWireType: unknown wire type " ++ show w

{-------------------------------------------------------------------------------
  Varint primitives
-------------------------------------------------------------------------------}

encodeVarint :: Word64 -> B.Builder
encodeVarint n
  | n < 128   = B.word8 (fromIntegral n)
  | otherwise = B.word8 (fromIntegral (n .&. 0x7F .|. 0x80)) <> encodeVarint (n `shiftR` 7)

decodeVarint :: ByteString -> Either String (Word64, ByteString)
decodeVarint = go 0 0
  where
    go acc sh bs =
      case BS.uncons bs of
        Nothing -> Left "decodeVarint: unexpected end of input"
        Just (b, rest) ->
          let val = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` sh)
          in if b .&. 0x80 == 0
               then Right (val, rest)
               else go val (sh + 7) rest

encodeZigZag32 :: Int32 -> Word32
encodeZigZag32 n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 31))

decodeZigZag32 :: Word32 -> Int32
decodeZigZag32 n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))

encodeZigZag64 :: Int64 -> Word64
encodeZigZag64 n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))

decodeZigZag64 :: Word64 -> Int64
decodeZigZag64 n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))

encodeFieldHeader :: Word32 -> WireType -> B.Builder
encodeFieldHeader fieldNum wt =
    encodeVarint (fromIntegral ((fieldNum `shiftL` 3) .|. wireTypeToWord wt))

{-------------------------------------------------------------------------------
  ProtobufField Class and Instances
-------------------------------------------------------------------------------}

class ProtobufField a where
  protobufWireType :: Proxy a -> WireType
  encodeFieldPayload :: a -> B.Builder
  decodeFieldPayload :: WireType -> ByteString -> Either String a
  decodeMissingField :: Either String a
  decodeMissingField = Left "missing required field"
  isFieldOmitted :: a -> Bool
  isFieldOmitted _ = False

instance ProtobufField Bool where
  protobufWireType _ = WireVarint
  encodeFieldPayload b = encodeVarint (if b then 1 else 0)
  decodeFieldPayload WireVarint bs = do
    (w, _) <- decodeVarint bs
    return (w /= 0)
  decodeFieldPayload wt _ = Left $ "ProtobufField Bool: unexpected wire type " ++ show wt

instance ProtobufField Int where
  protobufWireType _ = WireVarint
  encodeFieldPayload n = encodeVarint (fromIntegral (encodeZigZag64 (fromIntegral n)))
  decodeFieldPayload WireVarint bs = do
    (w, _) <- decodeVarint bs
    return (fromIntegral (decodeZigZag64 w))
  decodeFieldPayload wt _ = Left $ "ProtobufField Int: unexpected wire type " ++ show wt

instance ProtobufField Int32 where
  protobufWireType _ = WireVarint
  encodeFieldPayload n = encodeVarint (fromIntegral (encodeZigZag32 n))
  decodeFieldPayload WireVarint bs = do
    (w, _) <- decodeVarint bs
    return (decodeZigZag32 (fromIntegral w))
  decodeFieldPayload wt _ = Left $ "ProtobufField Int32: unexpected wire type " ++ show wt

instance ProtobufField Int64 where
  protobufWireType _ = WireVarint
  encodeFieldPayload n = encodeVarint (encodeZigZag64 n)
  decodeFieldPayload WireVarint bs = do
    (w, _) <- decodeVarint bs
    return (decodeZigZag64 w)
  decodeFieldPayload wt _ = Left $ "ProtobufField Int64: unexpected wire type " ++ show wt

instance ProtobufField Word where
  protobufWireType _ = WireVarint
  encodeFieldPayload n = encodeVarint (fromIntegral n)
  decodeFieldPayload WireVarint bs = do
    (w, _) <- decodeVarint bs
    return (fromIntegral w)
  decodeFieldPayload wt _ = Left $ "ProtobufField Word: unexpected wire type " ++ show wt

instance ProtobufField Word32 where
  protobufWireType _ = WireVarint
  encodeFieldPayload n = encodeVarint (fromIntegral n)
  decodeFieldPayload WireVarint bs = do
    (w, _) <- decodeVarint bs
    return (fromIntegral w)
  decodeFieldPayload wt _ = Left $ "ProtobufField Word32: unexpected wire type " ++ show wt

instance ProtobufField Word64 where
  protobufWireType _ = WireVarint
  encodeFieldPayload n = encodeVarint n
  decodeFieldPayload WireVarint bs = do
    (w, _) <- decodeVarint bs
    return w
  decodeFieldPayload wt _ = Left $ "ProtobufField Word64: unexpected wire type " ++ show wt

instance ProtobufField Float where
  protobufWireType _ = WireFixed32
  encodeFieldPayload f = B.floatLE f
  decodeFieldPayload WireFixed32 bs
    | BS.length bs >= 4 = Right (LBS.toStrict (B.toLazyByteString (B.byteString bs)) `seq` runFloat bs)
    | otherwise = Left "ProtobufField Float: expected 4 bytes"
    where
      runFloat b =
        let w = fromIntegral (BS.index b 0)
              .|. (fromIntegral (BS.index b 1) `shiftL` 8)
              .|. (fromIntegral (BS.index b 2) `shiftL` 16)
              .|. (fromIntegral (BS.index b 3) `shiftL` 24) :: Word32
        in decodeFloatLE w
      decodeFloatLE :: Word32 -> Float
      decodeFloatLE w =
        -- Reinterpret word bits as float
        let intVal = fromIntegral w :: Word32
        in (case castWord32ToFloat intVal of f -> f)
  decodeFieldPayload wt _ = Left $ "ProtobufField Float: unexpected wire type " ++ show wt

castWord32ToFloat :: Word32 -> Float
castWord32ToFloat w =
  -- In Haskell base, runST or unsafePerformIO or encode/decode IEEE 754
  -- A simple fallback using standard decode:
  let sign = if testBit w 31 then (-1.0) else 1.0
      expo = fromIntegral ((w `shiftR` 23) .&. 0xFF) :: Int
      mant = w .&. 0x7FFFFF
  in if expo == 0
       then if mant == 0 then sign * 0.0 else sign * fromIntegral mant * (2 ** (-149))
       else if expo == 255
              then if mant == 0 then sign * (1 / 0) else (0 / 0)
              else sign * (1.0 + fromIntegral mant / 8388608.0) * (2 ** fromIntegral (expo - 127))

instance ProtobufField Double where
  protobufWireType _ = WireFixed64
  encodeFieldPayload d = B.doubleLE d
  decodeFieldPayload WireFixed64 bs
    | BS.length bs >= 8 =
        let w = fromIntegral (BS.index bs 0)
              .|. (fromIntegral (BS.index bs 1) `shiftL` 8)
              .|. (fromIntegral (BS.index bs 2) `shiftL` 16)
              .|. (fromIntegral (BS.index bs 3) `shiftL` 24)
              .|. (fromIntegral (BS.index bs 4) `shiftL` 32)
              .|. (fromIntegral (BS.index bs 5) `shiftL` 40)
              .|. (fromIntegral (BS.index bs 6) `shiftL` 48)
              .|. (fromIntegral (BS.index bs 7) `shiftL` 56) :: Word64
            sign = if testBit w 63 then (-1.0) else 1.0
            expo = fromIntegral ((w `shiftR` 52) .&. 0x7FF) :: Int
            mant = w .&. 0xFFFFFFFFFFFFF
        in if expo == 0
             then if mant == 0 then Right (sign * 0.0) else Right (sign * fromIntegral mant * (2 ** (-1074)))
             else if expo == 2047
                    then if mant == 0 then Right (sign * (1 / 0)) else Right (0 / 0)
                    else Right (sign * (1.0 + fromIntegral mant / 4503599627370496.0) * (2 ** fromIntegral (expo - 1023)))
    | otherwise = Left "ProtobufField Double: expected 8 bytes"
  decodeFieldPayload wt _ = Left $ "ProtobufField Double: unexpected wire type " ++ show wt

instance ProtobufField String where
  protobufWireType _ = WireLengthDelimited
  encodeFieldPayload s =
    let bs = TE.encodeUtf8 (T.pack s)
    in encodeVarint (fromIntegral (BS.length bs)) <> B.byteString bs
  decodeFieldPayload WireLengthDelimited bs =
    case TE.decodeUtf8' bs of
      Left err -> Left $ "ProtobufField String: utf8 decode error: " ++ show err
      Right t  -> Right (T.unpack t)
  decodeFieldPayload wt _ = Left $ "ProtobufField String: unexpected wire type " ++ show wt

instance ProtobufField Text where
  protobufWireType _ = WireLengthDelimited
  encodeFieldPayload t =
    let bs = TE.encodeUtf8 t
    in encodeVarint (fromIntegral (BS.length bs)) <> B.byteString bs
  decodeFieldPayload WireLengthDelimited bs =
    case TE.decodeUtf8' bs of
      Left err -> Left $ "ProtobufField Text: utf8 decode error: " ++ show err
      Right t  -> Right t
  decodeFieldPayload wt _ = Left $ "ProtobufField Text: unexpected wire type " ++ show wt

instance ProtobufField ByteString where
  protobufWireType _ = WireLengthDelimited
  encodeFieldPayload bs = encodeVarint (fromIntegral (BS.length bs)) <> B.byteString bs
  decodeFieldPayload WireLengthDelimited bs = Right bs
  decodeFieldPayload wt _ = Left $ "ProtobufField ByteString: unexpected wire type " ++ show wt

instance ProtobufField a => ProtobufField (Maybe a) where
  protobufWireType _ = protobufWireType (Proxy @a)
  encodeFieldPayload Nothing  = mempty
  encodeFieldPayload (Just x) = encodeFieldPayload x
  decodeFieldPayload wt bs = Just <$> decodeFieldPayload wt bs
  decodeMissingField = Right Nothing
  isFieldOmitted Nothing = True
  isFieldOmitted _       = False

{-------------------------------------------------------------------------------
  Generic Protobuf Serialization
-------------------------------------------------------------------------------}

-- | Default 1-indexed field numbering (1, 2, 3, ...)
defaultTagAssignment :: forall a. Generic a => Rep (K Word32) a
defaultTagAssignment = Rep.map (\ix -> K (fromIntegral (Rep.indexToInt ix + 1))) Rep.allIndices

gencodeProtobuf :: forall a. (Generic a, Constraints a ProtobufField) => a -> ByteString
gencodeProtobuf = gencodeProtobufWith (\_ (K tag) -> tag)

gencodeProtobufWith ::
     forall a.
     (Generic a, Constraints a ProtobufField)
  => (forall x. FieldMetadata x -> K Word32 x -> Word32)
  -> a
  -> ByteString
gencodeProtobufWith assignTag a =
    LBS.toStrict . B.toLazyByteString . mconcat $ encodedFields
  where
    md = metadata (Proxy @a)
    tags :: Rep (K Word32) a
    tags = Rep.zipWith (\fld (K tag) -> K (assignTag fld (K tag))) (recordFieldMetadata md) defaultTagAssignment

    -- Build with tags zipped
    encodedFields :: [B.Builder]
    encodedFields = Rep.collapse $
      Rep.zipWith
        (\(Pair (K tag) (Dict :: Dict ProtobufField x)) (I val) ->
            K $ if isFieldOmitted val
                  then mempty
                  else encodeFieldHeader tag (protobufWireType (Proxy @x)) <> encodeFieldPayload val
        )
        (Rep.zip tags (dict (Proxy @ProtobufField)))
        (from a)

gdecodeProtobuf :: forall a. (Generic a, Constraints a ProtobufField) => ByteString -> Either String a
gdecodeProtobuf = gdecodeProtobufWith (\_ (K tag) -> tag)

gdecodeProtobufWith ::
     forall a.
     (Generic a, Constraints a ProtobufField)
  => (forall x. FieldMetadata x -> K Word32 x -> Word32)
  -> ByteString
  -> Either String a
gdecodeProtobufWith assignTag inputBytes = do
    fieldMap <- parseWireFields inputBytes Map.empty
    to <$> Rep.sequenceA (aux fieldMap)
  where
    md = metadata (Proxy @a)
    tags :: Rep (K Word32) a
    tags = Rep.zipWith (\fld (K tag) -> K (assignTag fld (K tag))) (recordFieldMetadata md) defaultTagAssignment

    aux :: Map Word32 (WireType, ByteString) -> Rep (Either String :.: I) a
    aux fldMap =
      Rep.cmap (Proxy @ProtobufField)
        (\(K tag) -> Comp (I <$> getField tag fldMap))
        tags

    getField :: forall x. ProtobufField x => Word32 -> Map Word32 (WireType, ByteString) -> Either String x
    getField tag fldMap =
      case Map.lookup tag fldMap of
        Just (wt, bs) -> decodeFieldPayload wt bs
        Nothing       -> case decodeMissingField of
          Right defVal -> Right defVal
          Left err     -> Left $ "gdecodeProtobuf: field " ++ show tag ++ ": " ++ err

    parseWireFields :: ByteString -> Map Word32 (WireType, ByteString) -> Either String (Map Word32 (WireType, ByteString))
    parseWireFields bs acc
      | BS.null bs = Right acc
      | otherwise = do
          (header, bs1) <- decodeVarint bs
          let fieldNum = fromIntegral (header `shiftR` 3) :: Word32
              wtWord   = fromIntegral (header .&. 7) :: Word32
          wt <- wordToWireType wtWord
          (payload, bs2) <- extractPayload wt bs1
          parseWireFields bs2 (Map.insert fieldNum (wt, payload) acc)

    extractPayload :: WireType -> ByteString -> Either String (ByteString, ByteString)
    extractPayload WireVarint bs = do
      (_, rest) <- decodeVarint bs
      let consumed = BS.length bs - BS.length rest
      Right (BS.take consumed bs, rest)
    extractPayload WireFixed64 bs
      | BS.length bs >= 8 = Right (BS.splitAt 8 bs)
      | otherwise = Left "Protobuf WireFixed64: unexpected end of input"
    extractPayload WireLengthDelimited bs = do
      (len, bs1) <- decodeVarint bs
      let lenI = fromIntegral len
      if BS.length bs1 >= lenI
        then Right (BS.splitAt lenI bs1)
        else Left "Protobuf WireLengthDelimited: unexpected end of payload"
    extractPayload WireFixed32 bs
      | BS.length bs >= 4 = Right (BS.splitAt 4 bs)
      | otherwise = Left "Protobuf WireFixed32: unexpected end of input"
