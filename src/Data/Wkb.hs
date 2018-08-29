module Data.Wkb where

import qualified Data.Binary.Get             as BinaryGet
import qualified Data.ByteString.Lazy        as LazyByteString
import qualified Data.Geospatial             as Geospatial

import qualified Data.Wkb.Endian             as Endian
import qualified Data.Wkb.GeometryCollection as GeometryCollection

parseByteString :: LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseByteString byteString =
  let
    geoSpatialGeometryResult = do
      (unconsumedInput, _, endianType)
        <- BinaryGet.runGetOrFail Endian.getEndianType byteString
      (_, _, geoSpatialGeometry)
        <- BinaryGet.runGetOrFail (GeometryCollection.getGeoSpatialGeometry endianType) unconsumedInput
      pure geoSpatialGeometry
  in
    case geoSpatialGeometryResult of
      Left (_, _, err) -> Left $ "Could not parse wkb: " ++ err
      Right x          -> Right x
