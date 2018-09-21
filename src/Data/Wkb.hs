module Data.Wkb
  ( parseByteString
  ) where

import qualified Data.Binary.Get              as BinaryGet
import qualified Data.ByteString.Lazy         as LazyByteString
import qualified Data.Geospatial              as Geospatial

import qualified Data.Internal.Wkb.Geometry   as Geometry
import qualified Data.Internal.Wkb.Geospatial as WkbGeospatial

parseByteString :: LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseByteString byteString =
  case BinaryGet.runGetOrFail
        (WkbGeospatial.geospatialGeometry Geometry.geometryTypeWithCoords)
        byteString of
    Left (_, _, err)                 -> Left $ "Could not parse wkb: " ++ err
    Right (_, _, geoSpatialGeometry) -> Right geoSpatialGeometry
