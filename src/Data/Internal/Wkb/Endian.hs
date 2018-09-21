module Data.Internal.Wkb.Endian
  ( EndianType (..)
  , endianType
  , fourBytes
  , doubleBytes
  ) where

import qualified Control.Monad   as Monad
import qualified Data.Binary.Get as BinaryGet
import qualified Data.Word       as Word

data EndianType = LittleEndian | BigEndian deriving (Show, Eq)

endianType :: BinaryGet.Get EndianType
endianType = do
  byte <- BinaryGet.getWord8
  case byte of
    0 ->
      pure BigEndian
    1 ->
      pure LittleEndian
    _ ->
      Monad.fail "Invalid EndianType"

fourBytes :: EndianType -> BinaryGet.Get Word.Word32
fourBytes et =
  case et of
    LittleEndian ->
      BinaryGet.getWord32le
    BigEndian ->
      BinaryGet.getWord32be

doubleBytes :: EndianType -> BinaryGet.Get Double
doubleBytes et =
  case et of
    LittleEndian ->
      BinaryGet.getDoublele
    BigEndian ->
      BinaryGet.getDoublebe
