module Data.Internal.Wkb.Geospatial
  ( geospatialGeometry
  ) where

import qualified Data.Binary.Get                      as BinaryGet
import qualified Data.Geospatial                      as Geospatial

import qualified Data.Internal.Wkb.Endian             as Endian
import qualified Data.Internal.Wkb.Geometry           as Geometry
import qualified Data.Internal.Wkb.GeometryCollection as GeometryCollection
import qualified Data.Internal.Wkb.Line               as Line
import qualified Data.Internal.Wkb.Point              as Point
import qualified Data.Internal.Wkb.Polygon            as Polygon

geospatialGeometry :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType)
                      -> BinaryGet.Get Geospatial.GeospatialGeometry
geospatialGeometry getWkbGeom = do
  endianType <- Endian.endianType
  geometryTypeWithCoords <- getWkbGeom endianType
  let (Geometry.WkbGeom geomType coordType) = geometryTypeWithCoords
  getFeature geomType getWkbGeom endianType coordType

getFeature :: Geometry.GeometryType
           -> (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType)
           -> Endian.EndianType
           -> Geometry.CoordinateType
           -> BinaryGet.Get Geospatial.GeospatialGeometry
getFeature geomType getWkbGeom =
  case geomType of
    Geometry.Geometry           -> getNoGeometry
    Geometry.Point              -> Point.point
    Geometry.LineString         -> Line.line
    Geometry.Polygon            -> Polygon.polygon
    Geometry.MultiPoint         -> Point.multiPoint getWkbGeom
    Geometry.MultiLineString    -> Line.multiLine getWkbGeom
    Geometry.MultiPolygon       -> Polygon.multiPolygon getWkbGeom
    Geometry.GeometryCollection -> GeometryCollection.geometryCollection (geospatialGeometry getWkbGeom)

getNoGeometry :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getNoGeometry _ _ =
  pure Geospatial.NoGeometry

