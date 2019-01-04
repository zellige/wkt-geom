module Data.Internal.Wkb.Polygon
  ( getPolygon
  , getMultiPolygon
  , builderPolygon
  , builderMultiPolygon
  ) where

import qualified Control.Monad                        as Monad
import qualified Data.Binary.Get                      as BinaryGet
import qualified Data.ByteString.Builder              as ByteStringBuilder
import qualified Data.Foldable                        as Foldable
import qualified Data.Geospatial                      as Geospatial
import qualified Data.LinearRing                      as LinearRing
import           Data.Monoid                          ((<>))
import qualified Data.Sequence                        as Sequence

import qualified Data.Internal.Wkb.Endian             as Endian
import qualified Data.Internal.Wkb.Geometry           as Geometry
import qualified Data.Internal.Wkb.GeometryCollection as GeometryCollection
import qualified Data.Internal.Wkb.Point              as Point
import qualified Data.SeqHelper                       as SeqHelper

-- Binary parsers

getPolygon :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getPolygon endianType coordType = do
  geoPolygon <- getGeoPolygon endianType coordType
  pure $ Geospatial.Polygon geoPolygon

getMultiPolygon :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType) -> Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getMultiPolygon getWkbGeom endianType _ = do
  numberOfPolygons <- Endian.getFourBytes endianType
  geoPolygons <- Sequence.replicateM (fromIntegral numberOfPolygons) (GeometryCollection.getEnclosedFeature getWkbGeom Geometry.Polygon getGeoPolygon)
  pure $ Geospatial.MultiPolygon $ Geospatial.mergeGeoPolygons geoPolygons

getGeoPolygon :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPolygon
getGeoPolygon endianType coordType = do
  linearRings <- getLinearRings endianType coordType
  pure $ Geospatial.GeoPolygon linearRings

getLinearRings :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
getLinearRings endianType coordType = do
  numberOfRings <- Endian.getFourBytes endianType
  Sequence.replicateM (fromIntegral numberOfRings) (getLinearRing endianType coordType)

getLinearRing :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
getLinearRing endianType coordType = do
  numberOfPoints <- Endian.getFourBytes endianType
  if numberOfPoints >= 4 then do
    p1 <- Point.getCoordPoint endianType coordType
    p2 <- Point.getCoordPoint endianType coordType
    p3 <- Point.getCoordPoint endianType coordType
    pts@(_ Sequence.:|> lastS) <- Point.getCoordPoints endianType coordType (numberOfPoints - 3)
    if lastS == p1 then
      pure $ LinearRing.makeLinearRing p1 p2 p3 (SeqHelper.sequenceHead pts)
    else
      Monad.fail $
        "First and last points of linear ring are different: first="
         ++ show p1 ++ " last=" ++ show lastS
  else
    Monad.fail $
      "Must have at least four points for a linear ring: "
       ++ show numberOfPoints


-- Binary builders

builderPolygon :: Endian.EndianType -> Geospatial.GeoPolygon -> ByteStringBuilder.Builder
builderPolygon endianType (Geospatial.GeoPolygon linearRings) = do
  let coordType = Geometry.coordTypeOfLinearRings linearRings
  Endian.builderEndianType endianType
    <> Geometry.builderGeometryType endianType (Geometry.WkbGeom Geometry.Polygon coordType)
    <> Endian.builderFourBytes endianType (fromIntegral $ length linearRings)
    <> Foldable.foldMap (builderLinearRing endianType) linearRings

builderMultiPolygon :: Endian.EndianType -> Geospatial.GeoMultiPolygon -> ByteStringBuilder.Builder
builderMultiPolygon endianType (Geospatial.GeoMultiPolygon polygons) =
  Endian.builderEndianType endianType
    <> Geometry.builderGeometryType endianType (Geometry.WkbGeom Geometry.MultiPolygon Geometry.TwoD)
    <> Endian.builderFourBytes endianType (fromIntegral $ length polygons)
    <> Foldable.foldMap (builderPolygon endianType . Geospatial.GeoPolygon) polygons

builderLinearRing :: Endian.EndianType -> LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS -> ByteStringBuilder.Builder
builderLinearRing endianType linearRing = do
  let coordPoints = LinearRing.toSeq linearRing
      lastCoordPoint = LinearRing.ringHead linearRing
      lengthOfRing = fromIntegral $ length coordPoints + 1
  Endian.builderFourBytes endianType lengthOfRing
    <> Foldable.foldMap (Point.builderCoordPoint endianType) coordPoints
    <> Point.builderCoordPoint endianType lastCoordPoint
