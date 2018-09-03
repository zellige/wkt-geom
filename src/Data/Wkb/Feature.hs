module Data.Wkb.Feature where

import qualified Data.Binary.Get   as BinaryGet

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Geometry as Geometry

getFeature :: (Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get a) -> BinaryGet.Get a
getFeature getter = do
  endianType <- Endian.getEndianType
  geometryTypeWithCoords <- Geometry.getGeometryTypeWithCoords endianType
  let (Geometry.WkbGeom _ coordType) = geometryTypeWithCoords
  getter endianType coordType
