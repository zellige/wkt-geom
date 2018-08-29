module Data.Wkb.GeometryCollection where

import           Control.Monad     ((>>=))
import qualified Control.Monad     as Monad
import qualified Data.Binary.Get   as BinaryGet
import qualified Data.Geospatial   as Geospatial
import qualified Data.Int          as Int

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Geometry as Geometry
import qualified Data.Wkb.Line     as Line
import qualified Data.Wkb.Point    as Point
import qualified Data.Wkb.Polygon  as Polygon

getGeoSpatialGeometry :: Endian.EndianType -> BinaryGet.Get Geospatial.GeospatialGeometry
getGeoSpatialGeometry endianType = do
  geometryTypeWithCoords <- Geometry.getGeometryTypeWithCoords endianType
  let (Geometry.WkbGeom geomType coordType) = geometryTypeWithCoords
  getter geomType endianType coordType
  where
    getter geomType =
      case geomType of
        Geometry.WkbGeometry           -> getNoGeometry
        Geometry.WkbPoint              -> Point.getPoint
        Geometry.WkbLineString         -> Line.getLine
        Geometry.WkbPolygon            -> Polygon.getPolygon
        Geometry.WkbMultiPoint         -> Point.getMultiPoint
        Geometry.WkbMultiLineString    -> Line.getMultiLine
        Geometry.WkbMultiPolygon       -> Polygon.getMultiPolygon
        Geometry.WkbGeometryCollection -> getGeometryCollection

getNoGeometry :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getNoGeometry _ _ =
  pure Geospatial.NoGeometry

getGeometryCollection :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getGeometryCollection endianType _ =
  Endian.getFourBytes endianType >>= (getGeoSpatialGeometries endianType)

getGeoSpatialGeometries :: Endian.EndianType -> Int.Int32 -> BinaryGet.Get Geospatial.GeospatialGeometry
getGeoSpatialGeometries endianType numberOfGeometries = do
  geoSpatialGeometries <- Monad.forM [1..numberOfGeometries] (\_ -> getGeoSpatialGeometry endianType)
  pure $ Geospatial.Collection geoSpatialGeometries
