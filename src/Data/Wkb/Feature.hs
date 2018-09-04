module Data.Wkb.Feature where

import qualified Control.Monad     as Monad
import qualified Data.Binary.Get   as BinaryGet

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Geometry as Geometry

getEnclosedFeature :: (Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get feature)
                      -> (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryTypeWithCoords)
                      -> Geometry.WkbGeometryType
                      -> BinaryGet.Get feature
getEnclosedFeature getFeature getWkbGeom expectedGeometryType = do
  endianType <- Endian.getEndianType
  geometryTypeWithCoords <- getWkbGeom endianType
  let (Geometry.WkbGeom geoType coordType) = geometryTypeWithCoords
  if geoType == expectedGeometryType then
    getFeature endianType coordType
  else
    Monad.fail "Wrong geometry type of enclosed feature"
