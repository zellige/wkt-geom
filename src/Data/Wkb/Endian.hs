module Data.Wkb.Endian where

import qualified Control.Monad   as Monad
import qualified Data.Binary.Get as BinaryGet
import qualified Data.Int        as Int

data EndianType = LittleEndian | BigEndian deriving (Show, Eq)

getEndianType :: BinaryGet.Get EndianType
getEndianType = do
  byte <- BinaryGet.getWord8
  case byte of
    0 ->
      return BigEndian
    1 ->
      return LittleEndian
    _ ->
      Monad.fail "Invalid EndianType"

getFourBytes :: EndianType -> BinaryGet.Get Int.Int32
getFourBytes endianType =
  case endianType of
    LittleEndian ->
      BinaryGet.getInt32le
    BigEndian ->
      BinaryGet.getInt32be

getDouble :: EndianType -> BinaryGet.Get Double
getDouble endianType =
  case endianType of
    LittleEndian ->
      BinaryGet.getDoublele
    BigEndian ->
      BinaryGet.getDoublebe
