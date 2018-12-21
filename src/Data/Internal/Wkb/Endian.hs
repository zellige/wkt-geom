module Data.Internal.Wkb.Endian
  ( EndianType (..)
  , getEndianType
  , getFourBytes
  , getDoubleBytes
  , endianTypeToBuilder
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
getFourBytes et =
  case et of
    LittleEndian ->
      BinaryGet.getWord32le
    BigEndian ->
      BinaryGet.getWord32be

getDoubleBytes :: EndianType -> BinaryGet.Get Double
getDoubleBytes et =
  case et of
    LittleEndian ->
      BinaryGet.getDoublele
    BigEndian ->
      BinaryGet.getDoublebe


endianTypeToBuilder :: EndianType -> ByteStringBuilder.Builder
endianTypeToBuilder BigEndian    = ByteStringBuilder.word8 0
endianTypeToBuilder LittleEndian = ByteStringBuilder.word8 1
