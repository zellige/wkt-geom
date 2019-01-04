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

builderGeospatialGeometry :: Endian.EndianType -> Geospatial.GeospatialGeometry -> ByteStringBuilder.Builder
builderGeospatialGeometry endianType geospatialGeometry =
  case geospatialGeometry of
    Geospatial.NoGeometry                   -> Monoid.mempty
    Geospatial.Point geoPoint               -> Point.builderPoint endianType geoPoint
    Geospatial.Line geoLine                 -> Line.builderLine endianType geoLine
    Geospatial.Polygon geoPolygon           -> Polygon.builderPolygon endianType geoPolygon
    Geospatial.MultiPoint geoMultiPoint     -> Point.builderMultiPoint endianType geoMultiPoint
    Geospatial.MultiLine geoMultiLine       -> Line.builderMultiLine endianType geoMultiLine
    Geospatial.MultiPolygon geoMultiPolygon -> Polygon.builderMultiPolygon endianType geoMultiPolygon
    Geospatial.Collection geoCollection     -> builderGeometryCollection endianType geoCollection
  where builderGeometryCollection =
          GeometryCollection.builderGeometryCollection builderGeospatialGeometry
