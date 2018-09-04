module Data.Wkb.GeometryCollection where

import qualified Control.Monad     as Monad
import qualified Data.Binary.Get   as BinaryGet
import qualified Data.Geospatial   as Geospatial

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Geometry as Geometry
import qualified Data.Wkb.Line     as Line
import qualified Data.Wkb.Point    as Point
import qualified Data.Wkb.Polygon  as Polygon

getGeoSpatialGeometry :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryTypeWithCoords) -> BinaryGet.Get Geospatial.GeospatialGeometry
getGeoSpatialGeometry getWkbGeom = do
  endianType <- Endian.getEndianType
  geometryTypeWithCoords <- getWkbGeom endianType
  let (Geometry.WkbGeom geomType coordType) = geometryTypeWithCoords
  getter geomType endianType coordType
  where
    getter geomType =
      case geomType of
        Geometry.WkbGeometry           -> getNoGeometry
        Geometry.WkbPoint              -> Point.getPoint
        Geometry.WkbLineString         -> Line.getLine
        Geometry.WkbPolygon            -> Polygon.getPolygon
        Geometry.WkbMultiPoint         -> Point.getMultiPoint getWkbGeom
        Geometry.WkbMultiLineString    -> Line.getMultiLine getWkbGeom
        Geometry.WkbMultiPolygon       -> Polygon.getMultiPolygon getWkbGeom
        Geometry.WkbGeometryCollection -> getGeometryCollection getWkbGeom

getNoGeometry :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getNoGeometry _ _ =
  pure Geospatial.NoGeometry

getGeometryCollection :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryTypeWithCoords) -> Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getGeometryCollection getWkbGeom endianType _ = do
  numberOfGeometries <- Endian.getFourBytes endianType
  geoSpatialGeometries <- Monad.forM [1..numberOfGeometries] (const $ getGeoSpatialGeometry getWkbGeom)
  pure $ Geospatial.Collection geoSpatialGeometries
