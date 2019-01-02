module Data.Internal.Wkb.Point
  ( point
  , multiPoint
  , getCoordPoint
  , getCoordPoints
  , coordPointToBuilder
  ) where

import qualified Data.Binary.Get                      as BinaryGet
import qualified Data.ByteString.Builder              as ByteStringBuilder
import qualified Data.Geospatial                      as Geospatial
import           Data.Monoid                          ((<>))
import qualified Data.Monoid                          as Monoid
import qualified Data.Sequence                        as Sequence
import qualified Data.Word                            as Word

import qualified Data.Internal.Wkb.Endian             as Endian
import qualified Data.Internal.Wkb.Geometry           as Geometry
import qualified Data.Internal.Wkb.GeometryCollection as GeometryCollection

point :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
point endianType coordType = do
  gp <- geoPoint endianType coordType
  pure $ Geospatial.Point gp

multiPoint :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType) -> Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
multiPoint getWkbGeom endianType _ = do
  numberOfPoints <- Endian.getFourBytes endianType
  geoPoints <- Sequence.replicateM (fromIntegral numberOfPoints) (GeometryCollection.enclosedFeature getWkbGeom Geometry.Point geoPoint)
  pure $ Geospatial.MultiPoint $ Geospatial.mergeGeoPoints geoPoints

geoPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPoint
geoPoint endianType coordType = do
  p <- getCoordPoint endianType coordType
  pure $ Geospatial.GeoPoint p

getCoordPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPositionWithoutCRS
getCoordPoint endianType coordType =
  case coordType of
    Geometry.TwoD -> do
      x <- Endian.getDouble endianType
      y <- Endian.getDouble endianType
      pure $ Geospatial.GeoPointXY (Geospatial.PointXY x y)
    Geometry.Z -> do
      x <- Endian.getDouble endianType
      y <- Endian.getDouble endianType
      z <- Endian.getDouble endianType
      pure $ Geospatial.GeoPointXYZ (Geospatial.PointXYZ x y z)
    Geometry.M -> do
      x <- Endian.getDouble endianType
      y <- Endian.getDouble endianType
      m <- Endian.getDouble endianType
      pure $ Geospatial.GeoPointXYZ (Geospatial.PointXYZ x y m)
    Geometry.ZM -> do
      x <- Endian.getDouble endianType
      y <- Endian.getDouble endianType
      z <- Endian.getDouble endianType
      m <- Endian.getDouble endianType
      pure $ Geospatial.GeoPointXYZM (Geospatial.PointXYZM x y z m)

getCoordPoints :: Endian.EndianType -> Geometry.CoordinateType -> Word.Word32 -> BinaryGet.Get (Sequence.Seq Geospatial.GeoPositionWithoutCRS)
getCoordPoints endianType coordType numberOfPoints =
  Sequence.replicateM (fromIntegral numberOfPoints) (getCoordPoint endianType coordType)

coordPointToBuilder :: Endian.EndianType -> Geospatial.GeoPositionWithoutCRS -> ByteStringBuilder.Builder
coordPointToBuilder endianType coordPoint =
  case coordPoint of
    Geospatial.GeoEmpty ->
      Monoid.mempty
    Geospatial.GeoPointXY (Geospatial.PointXY x y)         ->
      doubleToBuilder x <> doubleToBuilder y
    Geospatial.GeoPointXYZ (Geospatial.PointXYZ x y z)     ->
      doubleToBuilder x <> doubleToBuilder y <> doubleToBuilder z
    Geospatial.GeoPointXYZM (Geospatial.PointXYZM x y z m) ->
      doubleToBuilder x <> doubleToBuilder y <> doubleToBuilder z <> doubleToBuilder m
  where doubleToBuilder = Endian.doubleToBuilder endianType
