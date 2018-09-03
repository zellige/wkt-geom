module Data.Wkb.Feature where

import qualified Control.Monad     as Monad
import qualified Data.Binary.Get   as BinaryGet

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Geometry as Geometry

getEnclosedFeature :: (Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get a) -> Geometry.WkbGeometryType -> BinaryGet.Get a
getEnclosedFeature getter geometryType = do
  endianType <- Endian.getEndianType
  geometryTypeWithCoords <- Geometry.getGeometryTypeWithCoords endianType
  let (Geometry.WkbGeom geoType coordType) = geometryTypeWithCoords
  if geoType == geometryType then
    getter endianType coordType
  else
    Monad.fail "Wrong geometry type of enclosed feature"
