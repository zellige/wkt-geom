module Data.Internal.Hex
  ( safeConvert
  ) where

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base16 as ByteStringBase16
import qualified Data.ByteString.Lazy   as LazyByteString
import qualified Data.Geospatial        as Geospatial

safeConvert :: (LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry) -> ByteString.ByteString -> Either String Geospatial.GeospatialGeometry
safeConvert f byteString =
  let
    decoded = fst $ ByteStringBase16.decode byteString
  in
    if ByteString.null decoded then
      Left "Invalid hex representation."
    else
      f $ LazyByteString.fromStrict decoded


