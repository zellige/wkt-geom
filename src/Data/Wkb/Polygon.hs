module Data.Wkb.Polygon where

import qualified Control.Monad               as Monad
import qualified Data.Binary.Get             as BinaryGet
import qualified Data.Geospatial             as Geospatial
import qualified Data.LinearRing             as LinearRing
import qualified Data.Vector                 as Vector

import qualified Data.Wkb.Endian             as Endian
import qualified Data.Wkb.Geometry           as Geometry
import qualified Data.Wkb.GeometryCollection as GeometryCollection
import qualified Data.Wkb.Point              as Point

getPolygon :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getPolygon endianType coordType = do
  geoPolygon <- getGeoPolygon endianType coordType
  pure $ Geospatial.Polygon geoPolygon

getMultiPolygon :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType) -> Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getMultiPolygon getWkbGeom endianType _ = do
  numberOfPolygons <- Endian.getFourBytes endianType
  geoPolygons <- Vector.generateM (fromIntegral numberOfPolygons) (const $ GeometryCollection.getEnclosedFeature getWkbGeom Geometry.Polygon getGeoPolygon)
  pure $ Geospatial.MultiPolygon $ Geospatial.mergeGeoPolygons geoPolygons

getGeoPolygon :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPolygon
getGeoPolygon endianType coordType = do
  linearRings <- getLinearRings endianType coordType
  pure $ Geospatial.GeoPolygon linearRings

getLinearRings :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get (Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
getLinearRings endianType coordType = do
  numberOfRings <- Endian.getFourBytes endianType
  Vector.generateM (fromIntegral numberOfRings) (const $ getLinearRing endianType coordType)

getLinearRing :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
getLinearRing endianType coordType = do
  numberOfPoints <- Endian.getFourBytes endianType
  if numberOfPoints >= 4 then do
    p1 <- Point.getCoordPoint endianType coordType
    p2 <- Point.getCoordPoint endianType coordType
    p3 <- Point.getCoordPoint endianType coordType
    pts <- Point.getCoordPoints endianType coordType (numberOfPoints - 3)
    if Vector.last pts == p1 then
      pure $ LinearRing.makeLinearRing p1 p2 p3 (Vector.init pts)
    else
      Monad.fail $
        "First and last points of linear ring are different: first="
         ++ show p1 ++ " last=" ++ show (Vector.last pts)
  else
    Monad.fail $
      "Must have at least four points for a linear ring: "
       ++ show numberOfPoints
