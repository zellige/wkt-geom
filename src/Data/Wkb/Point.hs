module Data.Wkb.Point where

import qualified Data.Binary.Get   as Get
import qualified Data.Geospatial   as Geospatial

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Geometry as Geometry

getPoint :: Endian.EndianType -> Geometry.WkbCoordinateType -> Get.Get Geospatial.GeospatialGeometry
getPoint endianType _ = do
  x <- Endian.getDouble endianType
  y <- Endian.getDouble endianType
  return $ Geospatial.Point $ Geospatial.GeoPoint [x,y]

