module Data.Wkb.Geospatial where

import qualified Data.Binary.Get             as BinaryGet
import qualified Data.Geospatial             as Geospatial

import qualified Data.Wkb.Endian             as Endian
import qualified Data.Wkb.Geometry           as Geometry
import qualified Data.Wkb.GeometryCollection as GeometryCollection
import qualified Data.Wkb.Line               as Line
import qualified Data.Wkb.Point              as Point
import qualified Data.Wkb.Polygon            as Polygon

getGeospatialGeometry :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType)
                      -> BinaryGet.Get Geospatial.GeospatialGeometry
getGeospatialGeometry getWkbGeom = do
  endianType <- Endian.getEndianType
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
    Geometry.Point              -> Point.getPoint
    Geometry.LineString         -> Line.getLine
    Geometry.Polygon            -> Polygon.getPolygon
    Geometry.MultiPoint         -> Point.getMultiPoint getWkbGeom
    Geometry.MultiLineString    -> Line.getMultiLine getWkbGeom
    Geometry.MultiPolygon       -> Polygon.getMultiPolygon getWkbGeom
    Geometry.GeometryCollection -> GeometryCollection.getGeometryCollection (getGeospatialGeometry getWkbGeom)

getNoGeometry :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getNoGeometry _ _ =
  pure Geospatial.NoGeometry

