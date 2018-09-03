module Data.Wkb.Endian where

import qualified Control.Monad   as Monad
import qualified Data.Binary.Get as BinaryGet
import qualified Data.Word       as Word

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
