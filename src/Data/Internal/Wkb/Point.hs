module Data.Internal.Wkb.Point
  ( getPoint
  , getMultiPoint
  , getCoordPoint
  , getCoordPoints
  , builderCoordPoint
  , builderCoordPoints
  ) where

import qualified Data.Binary.Get                      as BinaryGet
import qualified Data.ByteString.Builder              as ByteStringBuilder
import qualified Data.Foldable                        as Foldable
import qualified Data.Geospatial                      as Geospatial
import qualified Data.Monoid                          as Monoid
import qualified Data.Sequence                        as Sequence
import qualified Data.Word                            as Word

import qualified Data.Internal.Wkb.Endian             as Endian
import qualified Data.Internal.Wkb.Geometry           as Geometry
import qualified Data.Internal.Wkb.GeometryCollection as GeometryCollection

getPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getPoint endianType coordType = do
  gp <- getGeoPoint endianType coordType
  pure $ Geospatial.Point gp

getMultiPoint :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType) -> Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getMultiPoint getWkbGeom endianType _ = do
  numberOfPoints <- Endian.getFourBytes endianType
  geoPoints <- Sequence.replicateM (fromIntegral numberOfPoints) (GeometryCollection.enclosedFeature getWkbGeom Geometry.Point getGeoPoint)
  pure $ Geospatial.MultiPoint $ Geospatial.mergeGeoPoints geoPoints

getGeoPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPoint
getGeoPoint endianType coordType = do
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

builderCoordPoint :: Endian.EndianType -> Geospatial.GeoPositionWithoutCRS -> ByteStringBuilder.Builder
builderCoordPoint endianType coordPoint =
  case coordPoint of
    Geospatial.GeoEmpty ->
      Monoid.mempty
    Geospatial.GeoPointXY (Geospatial.PointXY x y)         ->
      Foldable.foldMap builderDouble [x, y]
    Geospatial.GeoPointXYZ (Geospatial.PointXYZ x y z)     ->
      Foldable.foldMap builderDouble [x, y, z]
    Geospatial.GeoPointXYZM (Geospatial.PointXYZM x y z m) ->
      Foldable.foldMap builderDouble [x, y, z, m]
  where builderDouble = Endian.builderDouble endianType

builderCoordPoints :: Endian.EndianType -> Sequence.Seq Geospatial.GeoPositionWithoutCRS -> ByteStringBuilder.Builder
builderCoordPoints endianType coordPoints =
  Foldable.foldMap (builderCoordPoint endianType) coordPoints
