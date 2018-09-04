module Data.Ewkb where

import qualified Data.Binary.Get      as BinaryGet
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Geospatial      as Geospatial

import qualified Data.Ewkb.Geometry   as EwkbGeometry
import qualified Data.Wkb.Geospatial  as WkbGeospatial

parseByteString :: LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseByteString byteString =
  case BinaryGet.runGetOrFail
        (WkbGeospatial.getGeospatialGeometry EwkbGeometry.getWkbGeometryType)
        byteString of
    Left (_, _, err)                 -> Left $ "Could not parse ewkb: " ++ err
    Right (_, _, geoSpatialGeometry) -> Right geoSpatialGeometry
