module Data.Internal.Wkb.GeometryCollection
  ( getGeometryCollection
  , getEnclosedFeature
  , builderGeometryCollection
  ) where

import qualified Control.Monad              as Monad
import qualified Data.Binary.Get            as BinaryGet
import qualified Data.ByteString.Builder    as ByteStringBuilder
import qualified Data.Foldable              as Foldable
import qualified Data.Geospatial            as Geospatial
import qualified Data.Sequence              as Sequence

import qualified Data.Internal.Wkb.Endian   as Endian
import qualified Data.Internal.Wkb.Geometry as Geometry


-- Binary parsers

getGeometryCollection :: BinaryGet.Get Geospatial.GeospatialGeometry
                          -> Endian.EndianType
                          -> Geometry.CoordinateType
                          -> BinaryGet.Get Geospatial.GeospatialGeometry
getGeometryCollection getGeospatialGeometry endianType _ = do
  numberOfGeometries <- Endian.getFourBytes endianType
  geoSpatialGeometries <- Sequence.replicateM (fromIntegral numberOfGeometries) getGeospatialGeometry
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


-- Binary builders

builderGeometryCollection :: (Endian.EndianType -> Geospatial.GeospatialGeometry -> ByteStringBuilder.Builder)
                             -> Endian.EndianType
                             -> Sequence.Seq Geospatial.GeospatialGeometry
                             -> ByteStringBuilder.Builder
builderGeometryCollection builderGeospatialFeature endianType geometryCollection =
  Endian.builderFourBytes endianType (fromIntegral $ length geometryCollection)
    <> Foldable.foldMap (builderGeospatialFeature endianType) geometryCollection
