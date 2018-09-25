module Data.Internal.Wkb.Polygon
  ( polygon
  , multiPolygon
  ) where

import qualified Control.Monad                        as Monad
import qualified Data.Binary.Get                      as BinaryGet
import qualified Data.Geospatial                      as Geospatial
import qualified Data.LinearRing                      as LinearRing
import qualified Data.Sequence                        as Sequence

import qualified Data.Internal.Wkb.Endian             as Endian
import qualified Data.Internal.Wkb.Geometry           as Geometry
import qualified Data.Internal.Wkb.GeometryCollection as GeometryCollection
import qualified Data.Internal.Wkb.Point              as Point

polygon :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
polygon endianType coordType = do
  geoPolygon <- getGeoPolygon endianType coordType
  pure $ Geospatial.Polygon geoPolygon

multiPolygon :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType) -> Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
multiPolygon getWkbGeom endianType _ = do
  numberOfPolygons <- Endian.fourBytes endianType
  geoPolygons <- Sequence.replicateM (fromIntegral numberOfPolygons) (GeometryCollection.enclosedFeature getWkbGeom Geometry.Polygon getGeoPolygon)
  pure $ Geospatial.MultiPolygon $ Geospatial.mergeGeoPolygons geoPolygons

getGeoPolygon :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPolygon
getGeoPolygon endianType coordType = do
  linearRings <- getLinearRings endianType coordType
  pure $ Geospatial.GeoPolygon linearRings

getLinearRings :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
getLinearRings endianType coordType = do
  numberOfRings <- Endian.fourBytes endianType
  Sequence.replicateM (fromIntegral numberOfRings) (getLinearRing endianType coordType)

getLinearRing :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
getLinearRing endianType coordType = do
  numberOfPoints <- Endian.fourBytes endianType
  if numberOfPoints >= 4 then do
    p1 <- Point.coordPoint endianType coordType
    p2 <- Point.coordPoint endianType coordType
    p3 <- Point.coordPoint endianType coordType
    pts@(_ Sequence.:|> lastS) <- Point.coordPoints endianType coordType (numberOfPoints - 3)
    if lastS == p1 then
      pure $ LinearRing.makeLinearRing p1 p2 p3 (sequenceHead pts)
    else
      Monad.fail $
        "First and last points of linear ring are different: first="
         ++ show p1 ++ " last=" ++ show lastS
  else
    Monad.fail $
      "Must have at least four points for a linear ring: "
       ++ show numberOfPoints

-- All but the last
sequenceHead :: Sequence.Seq a -> Sequence.Seq a
sequenceHead (headS Sequence.:|> _) = headS
sequenceHead _                      = Sequence.empty
