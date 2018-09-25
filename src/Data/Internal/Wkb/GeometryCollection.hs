module Data.Internal.Wkb.GeometryCollection
  ( geometryCollection
  , enclosedFeature
  ) where

import qualified Control.Monad              as Monad
import qualified Data.Binary.Get            as BinaryGet
import qualified Data.Geospatial            as Geospatial
import qualified Data.Sequence              as Sequence

import qualified Data.Internal.Wkb.Endian   as Endian
import qualified Data.Internal.Wkb.Geometry as Geometry

geometryCollection :: BinaryGet.Get Geospatial.GeospatialGeometry
                          -> Endian.EndianType
                          -> Geometry.CoordinateType
                          -> BinaryGet.Get Geospatial.GeospatialGeometry
geometryCollection getGeospatialGeometry endianType _ = do
  numberOfGeometries <- Endian.fourBytes endianType
  geoSpatialGeometries <- Sequence.replicateM (fromIntegral numberOfGeometries) getGeospatialGeometry
  pure $ Geospatial.Collection geoSpatialGeometries

enclosedFeature :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType)
                      -> Geometry.GeometryType
                      -> (Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get feature)
                      -> BinaryGet.Get feature
enclosedFeature getWkbGeom expectedGeometryType getFeature = do
  endianType <- Endian.endianType
  geometryTypeWithCoords <- getWkbGeom endianType
  let (Geometry.WkbGeom geoType coordType) = geometryTypeWithCoords
  if geoType == expectedGeometryType then
    getFeature endianType coordType
  else
    Monad.fail "Wrong geometry type of enclosed feature"
