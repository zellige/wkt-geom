module Data.Wkb where

import qualified Data.Binary.Get             as BinaryGet
import qualified Data.ByteString.Lazy        as LazyByteString
import qualified Data.Geospatial             as Geospatial

import qualified Data.Wkb.GeometryCollection as GeometryCollection

parseByteString :: LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseByteString byteString =
  case BinaryGet.runGetOrFail
        GeometryCollection.getGeoSpatialGeometry
        byteString of
    Left (_, _, err)                 -> Left $ "Could not parse wkb: " ++ err
    Right (_, _, geoSpatialGeometry) -> Right geoSpatialGeometry
