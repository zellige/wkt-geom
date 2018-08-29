module Data.Wkb.Line where

import qualified Control.Monad     as Monad
import qualified Data.Binary.Get   as BinaryGet
import qualified Data.Geospatial   as Geospatial
import qualified Data.Int          as Int
import qualified Data.LineString   as LineString

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Geometry as Geometry
import qualified Data.Wkb.Point    as Point

getLine :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getLine endianType coordType = do
  geoLine <- getGeoLine endianType coordType
  pure $ Geospatial.Line geoLine

getMultiLine :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getMultiLine endianType coordType = do
  geoLines <- Endian.getFourBytes endianType >>= (getGeoLines endianType coordType)
  pure $ Geospatial.MultiLine $ Geospatial.mergeGeoLines geoLines

getGeoLines :: Endian.EndianType -> Geometry.WkbCoordinateType -> Int.Int32 -> BinaryGet.Get [Geospatial.GeoLine]
getGeoLines endianType coordType numberOfPoints =
  Monad.forM [1..numberOfPoints] (\_ -> getGeoLine endianType coordType)

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
