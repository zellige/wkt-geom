module Data.Wkb.GeometryCollection where

import qualified Control.Monad     as Monad
import qualified Data.Binary.Get   as BinaryGet
import qualified Data.Geospatial   as Geospatial
import qualified Data.Vector       as Vector

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Geometry as Geometry

getGeometryCollection :: BinaryGet.Get Geospatial.GeospatialGeometry
                          -> Endian.EndianType
                          -> Geometry.CoordinateType
                          -> BinaryGet.Get Geospatial.GeospatialGeometry
getGeometryCollection getGeospatialGeometry endianType _ = do
  numberOfGeometries <- Endian.getFourBytes endianType
  geoSpatialGeometries <- Vector.generateM (fromIntegral numberOfGeometries) (const getGeospatialGeometry)
  pure $ Geospatial.Collection geoSpatialGeometries

getEnclosedFeature :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType)
                      -> Geometry.GeometryType
                      -> (Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get feature)
                      -> BinaryGet.Get feature
getEnclosedFeature getWkbGeom expectedGeometryType getFeature = do
  endianType <- Endian.getEndianType
  geometryTypeWithCoords <- getWkbGeom endianType
  let (Geometry.WkbGeom geoType coordType) = geometryTypeWithCoords
  if geoType == expectedGeometryType then
    getFeature endianType coordType
  else
    Monad.fail "Wrong geometry type of enclosed feature"
