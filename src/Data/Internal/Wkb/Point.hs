module Data.Internal.Wkb.Point
  ( point
  , multiPoint
  , coordPoint
  , coordPoints
  ) where

import qualified Data.Binary.Get                      as BinaryGet
import qualified Data.Geospatial                      as Geospatial
import qualified Data.Sequence                        as Sequence
import qualified Data.Vector.Storable                 as VectorStorable
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
  numberOfPoints <- Endian.fourBytes endianType
  geoPoints <- Sequence.replicateM (fromIntegral numberOfPoints) (GeometryCollection.enclosedFeature getWkbGeom Geometry.Point geoPoint)
  pure $ Geospatial.MultiPoint $ Geospatial.mergeGeoPoints geoPoints

geoPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPoint
geoPoint endianType coordType = do
  p <- coordPoint endianType coordType
  pure $ Geospatial.GeoPoint p

coordPoints :: Endian.EndianType -> Geometry.CoordinateType -> Word.Word32 -> BinaryGet.Get (Sequence.Seq Geospatial.GeoPositionWithoutCRS)
coordPoints endianType coordType numberOfPoints =
  Sequence.replicateM (fromIntegral numberOfPoints) (coordPoint endianType coordType)

coordPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPositionWithoutCRS
coordPoint endianType coordType =
  case coordType of
    Geometry.TwoD -> do
      x <- Endian.doubleBytes endianType
      y <- Endian.doubleBytes endianType
      pure $ Geospatial.GeoPointXY (Geospatial.PointXY x y)
    Geometry.Z -> do
      x <- Endian.doubleBytes endianType
      y <- Endian.doubleBytes endianType
      z <- Endian.doubleBytes endianType
      pure $ Geospatial.GeoPointXYZ (Geospatial.PointXYZ x y z)
    Geometry.M -> do
      x <- Endian.doubleBytes endianType
      y <- Endian.doubleBytes endianType
      m <- Endian.doubleBytes endianType
      pure $ Geospatial.GeoPointXYZ (Geospatial.PointXYZ x y m)
    Geometry.ZM -> do
      x <- Endian.doubleBytes endianType
      y <- Endian.doubleBytes endianType
      z <- Endian.doubleBytes endianType
      m <- Endian.doubleBytes endianType
      pure $ Geospatial.GeoPointXYZM (Geospatial.PointXYZM x y z m)
