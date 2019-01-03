module Data.Hex
  ( safeConvert
  , Hex(..)
  ) where

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base16 as ByteStringBase16
import qualified Data.ByteString.Char8  as ByteStringChar8
import qualified Data.ByteString.Lazy   as LazyByteString
import qualified Data.Geospatial        as Geospatial
import           Data.Monoid            ((<>))

newtype Hex = Hex ByteString.ByteString

safeConvert :: (LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry) -> Hex -> Either String Geospatial.GeospatialGeometry
safeConvert f (Hex byteString) =
  let
    (decoded, rest) = ByteStringBase16.decode byteString
  in
    if ByteString.null rest then
      f $ LazyByteString.fromStrict decoded
    else
      Left ("Invalid hex representation: " <> ByteStringChar8.unpack byteString)


