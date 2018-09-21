module Data.Internal.Wkb.Line
  ( Data.Internal.Wkb.Line.line
  , multiLine
  ) where

import qualified Control.Monad                        as Monad
import qualified Data.Binary.Get                      as BinaryGet
import qualified Data.Geospatial                      as Geospatial
import qualified Data.LineString                      as LineString
import qualified Data.Vector                          as Vector

import qualified Data.Internal.Wkb.Endian             as Endian
import qualified Data.Internal.Wkb.Geometry           as Geometry
import qualified Data.Internal.Wkb.GeometryCollection as GeometryCollection
import qualified Data.Internal.Wkb.Point              as Point

line :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
line endianType coordType = do
  gl <- geoLine endianType coordType
  pure $ Geospatial.Line gl

multiLine :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType) -> Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
multiLine getWkbGeom endianType _ = do
  numberOfLines <- Endian.fourBytes endianType
  geoLines <- Vector.generateM (fromIntegral numberOfLines) (const $ GeometryCollection.enclosedFeature getWkbGeom Geometry.LineString geoLine)
  pure $ Geospatial.MultiLine $ Geospatial.mergeGeoLines geoLines

geoLine :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoLine
geoLine endianType coordType = do
  numberOfPoints <- Endian.fourBytes endianType
  if numberOfPoints >= 2 then do
    p1 <- Point.coordPoint endianType coordType
    p2 <- Point.coordPoint endianType coordType
    pts <- Point.coordPoints endianType coordType (numberOfPoints - 2)
    pure $ Geospatial.GeoLine $ LineString.makeLineString p1 p2 pts
  else
    Monad.fail "Must have at least two points for a line"
