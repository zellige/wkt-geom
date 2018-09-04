module Data.Wkb.Line where

import qualified Control.Monad     as Monad
import qualified Data.Binary.Get   as BinaryGet
import qualified Data.Geospatial   as Geospatial
import qualified Data.LineString   as LineString

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Feature  as Feature
import qualified Data.Wkb.Geometry as Geometry
import qualified Data.Wkb.Point    as Point

getLine :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getLine endianType coordType = do
  geoLine <- getGeoLine endianType coordType
  pure $ Geospatial.Line geoLine

getMultiLine :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryTypeWithCoords) -> Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getMultiLine getWkbGeom endianType _ = do
  numberOfLines <- Endian.getFourBytes endianType
  geoLines <- Monad.forM [1..numberOfLines] (const $ Feature.getEnclosedFeature getGeoLine getWkbGeom Geometry.WkbLineString)
  pure $ Geospatial.MultiLine $ Geospatial.mergeGeoLines geoLines

getGeoLine :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeoLine
getGeoLine endianType coordType = do
  numberOfPoints <- Endian.getFourBytes endianType
  if numberOfPoints >= 2 then do
    p1 <- Point.getCoordPoint endianType coordType
    p2 <- Point.getCoordPoint endianType coordType
    pts <- Point.getCoordPoints endianType coordType (numberOfPoints - 2)
    pure $ Geospatial.GeoLine $ LineString.makeLineString p1 p2 pts
  else
    Monad.fail "Must have at least two points for a line"
