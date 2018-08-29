module Data.Wkb.Endian where

import qualified Control.Monad   as Monad
import qualified Data.Binary.Get as Get
import qualified Data.Int        as Int

data EndianType = LittleEndian | BigEndian deriving (Show, Eq)

getEndianType :: Get.Get EndianType
getEndianType = do
  byte <- Get.getWord8
  case byte of
    0 ->
      pure BigEndian
    1 ->
      pure LittleEndian
    _ ->
      Monad.fail "Invalid EndianType"

getFourBytes :: EndianType -> Get.Get Int.Int32
getFourBytes endianType =
  case endianType of
    LittleEndian ->
      Get.getInt32le
    BigEndian ->
      Get.getInt32be

getDouble :: EndianType -> Get.Get Double
getDouble endianType =
  case endianType of
    LittleEndian ->
      Get.getDoublele
    BigEndian ->
      Get.getDoublebe
