module Data.Wkb where

import qualified Data.Binary.Get      as BinaryGet
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Geospatial      as Geospatial

import qualified Data.Wkb.Geometry    as Geometry
import qualified Data.Wkb.Geospatial  as WkbGeospatial

parseByteString :: LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseByteString byteString =
  case BinaryGet.runGetOrFail
        (WkbGeospatial.getGeospatialGeometry Geometry.getGeometryTypeWithCoords)
        byteString of
    Left (_, _, err)                 -> Left $ "Could not parse wkb: " ++ err
    Right (_, _, geoSpatialGeometry) -> Right geoSpatialGeometry
