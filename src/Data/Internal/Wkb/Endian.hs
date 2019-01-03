module Data.Internal.Wkb.Endian
  ( EndianType (..)
  , getEndianType
  , getFourBytes
  , getDouble
  , builderEndianType
  , builderFourBytes
  , builderDouble
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

builderEndianType :: EndianType -> ByteStringBuilder.Builder
builderEndianType BigEndian    = ByteStringBuilder.word8 0
builderEndianType LittleEndian = ByteStringBuilder.word8 1

builderFourBytes :: EndianType -> Word.Word32 -> ByteStringBuilder.Builder
builderFourBytes endianType =
  case endianType of
    LittleEndian ->
      ByteStringBuilder.word32LE
    BigEndian ->
      ByteStringBuilder.word32BE

builderDouble :: EndianType -> Double -> ByteStringBuilder.Builder
builderDouble endianType =
  case endianType of
    LittleEndian ->
      ByteStringBuilder.doubleLE
    BigEndian ->
      ByteStringBuilder.doubleBE
