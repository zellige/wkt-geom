module Data.Internal.Wkb.Geospatial
  ( getGeospatialGeometry
  , builderGeospatialGeometry
  ) where

import qualified Data.Binary.Get                      as BinaryGet
import qualified Data.ByteString.Builder              as ByteStringBuilder
import qualified Data.Geospatial                      as Geospatial
import qualified Data.Monoid                          as Monoid

import qualified Data.Internal.Wkb.Endian             as Endian
import qualified Data.Internal.Wkb.Geometry           as Geometry
import qualified Data.Internal.Wkb.GeometryCollection as GeometryCollection
import qualified Data.Internal.Wkb.Line               as Line
import qualified Data.Internal.Wkb.Point              as Point
import qualified Data.Internal.Wkb.Polygon            as Polygon


-- Binary parsers

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


-- Binary builders

builderGeospatialGeometry :: Geometry.BuilderWkbGeometryType
                          -> Endian.EndianType
                          -> Geospatial.GeospatialGeometry
                          -> ByteStringBuilder.Builder
builderGeospatialGeometry builderWkbGeom endianType geospatialGeometry =
  case geospatialGeometry of
    Geospatial.NoGeometry                   -> Monoid.mempty
    Geospatial.Point geoPoint               -> build Point.builderPoint geoPoint
    Geospatial.Line geoLine                 -> build Line.builderLine geoLine
    Geospatial.Polygon geoPolygon           -> build Polygon.builderPolygon geoPolygon
    Geospatial.MultiPoint geoMultiPoint     -> build Point.builderMultiPoint geoMultiPoint
    Geospatial.MultiLine geoMultiLine       -> build Line.builderMultiLine geoMultiLine
    Geospatial.MultiPolygon geoMultiPolygon -> build Polygon.builderMultiPolygon geoMultiPolygon
    Geospatial.Collection geoCollection     -> builderGeometryCollection endianType geoCollection
  where builderGeometryCollection =
          GeometryCollection.builderGeometryCollection builderGeospatialGeometry builderWkbGeom
        build builder = builder builderWkbGeom endianType
