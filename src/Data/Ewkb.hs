-- Refer to the eWKB Postgis Documentation <https://postgis.net/docs/using_postgis_dbmanagement.html#EWKB_EWKT>
--
-- Allows parsing of ByteString into a Geospatial Object.
--
-------------------------------------------------------------------
module Data.Ewkb
  ( parseByteString
  ) where

import qualified Data.Binary.Get              as BinaryGet
import qualified Data.ByteString.Lazy         as LazyByteString
import qualified Data.Geospatial              as Geospatial

import qualified Data.Internal.Ewkb.Geometry  as EwkbGeometry
import qualified Data.Internal.Wkb.Geospatial as WkbGeospatial

parseByteString :: LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseByteString byteString =
  case BinaryGet.runGetOrFail
        (WkbGeospatial.geospatialGeometry EwkbGeometry.wkbGeometryType)
        byteString of
    Left (_, _, err)                 -> Left $ "Could not parse ewkb: " ++ err
    Right (_, _, geoSpatialGeometry) -> Right geoSpatialGeometry
