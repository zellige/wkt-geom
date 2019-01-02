module Data.Internal.Wkb.Endian
  ( EndianType (..)
  , getEndianType
  , getFourBytes
  , getDouble
  , endianTypeToBuilder
  , fourBytesToBuilder
  , doubleToBuilder
  ) where

import qualified Control.Monad           as Monad
import qualified Data.Binary.Get         as BinaryGet
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.Word               as Word

data EndianType = LittleEndian | BigEndian deriving (Show, Eq)

getEndianType :: BinaryGet.Get EndianType
getEndianType = do
  byte <- BinaryGet.getWord8
  case byte of
    0 ->
      pure BigEndian
    1 ->
      pure LittleEndian
    _ ->
      Monad.fail "Invalid EndianType"

getFourBytes :: EndianType -> BinaryGet.Get Word.Word32
getFourBytes endianType =
  case endianType of
    LittleEndian ->
      BinaryGet.getWord32le
    BigEndian ->
      BinaryGet.getWord32be

getDouble :: EndianType -> BinaryGet.Get Double
getDouble endianType =
  case endianType of
    LittleEndian ->
      BinaryGet.getDoublele
    BigEndian ->
      BinaryGet.getDoublebe

endianTypeToBuilder :: EndianType -> ByteStringBuilder.Builder
endianTypeToBuilder BigEndian    = ByteStringBuilder.word8 0
endianTypeToBuilder LittleEndian = ByteStringBuilder.word8 1

fourBytesToBuilder :: EndianType -> Word.Word32 -> ByteStringBuilder.Builder
fourBytesToBuilder endianType =
  case endianType of
    LittleEndian ->
      ByteStringBuilder.word32LE
    BigEndian ->
      ByteStringBuilder.word32BE

doubleToBuilder :: EndianType -> Double -> ByteStringBuilder.Builder
doubleToBuilder endianType =
  case endianType of
    LittleEndian ->
      ByteStringBuilder.doubleLE
    BigEndian ->
      ByteStringBuilder.doubleBE
