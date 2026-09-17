-- | Protocol Buffers wire format serialization for large records
module Data.Record.Protobuf (
    -- * Field encoding
    ProtobufField(..)
  , WireType(..)
    -- * Generic codecs
  , gencodeProtobuf
  , gdecodeProtobuf
  , gencodeProtobufWith
  , gdecodeProtobufWith
  ) where

import Data.Record.Generic.Protobuf
