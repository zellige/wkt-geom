-- Refer to the eWKB Postgis Documentation <https://postgis.net/docs/using_postgis_dbmanagement.html#EWKB_EWKT>
--
-- Allows parsing of ByteString into a Geospatial Object.
--
-------------------------------------------------------------------
module Data.Ewkb
  ( parseByteString
  , parseHexByteString
  ) where

import qualified Data.Binary.Get              as BinaryGet
import qualified Data.ByteString.Lazy         as LazyByteString
import qualified Data.Geospatial              as Geospatial

import qualified Data.Internal.Ewkb.Geometry  as EwkbGeometry
import qualified Data.Internal.Wkb.Geospatial as WkbGeospatial

import qualified Data.Hex                     as Hex

-- |
-- Representation of EWKB as Binary
parseByteString :: LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry
parseByteString byteString =
  case BinaryGet.runGetOrFail
        (WkbGeospatial.getGeospatialGeometry EwkbGeometry.wkbGeometryType)
        byteString of
    Left (_, _, err)                 -> Left $ "Could not parse ewkb: " ++ err
    Right (_, _, geoSpatialGeometry) -> Right geoSpatialGeometry

-- |
-- Representation of EWKB as a String in Base16/Hex form i.e. "0101000000000000000000f03f0000000000000040" is POINT 1.0 2.0
parseHexByteString :: Hex.Hex -> Either String Geospatial.GeospatialGeometry
parseHexByteString = Hex.safeConvert parseByteString
