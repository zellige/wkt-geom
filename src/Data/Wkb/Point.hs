module Data.Wkb.Point where

import qualified Data.Binary.Get             as BinaryGet
import qualified Data.Geospatial             as Geospatial
import qualified Data.Vector                 as Vector
import qualified Data.Word                   as Word

import qualified Data.Wkb.Endian             as Endian
import qualified Data.Wkb.Geometry           as Geometry
import qualified Data.Wkb.GeometryCollection as GeometryCollection

getPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getPoint endianType coordType = do
  geoPoint <- getGeoPoint endianType coordType
  pure $ Geospatial.Point geoPoint

getMultiPoint :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType) -> Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getMultiPoint getWkbGeom endianType _ = do
  numberOfPoints <- Endian.getFourBytes endianType
  geoPoints <- Vector.generateM (fromIntegral numberOfPoints) (const $ GeometryCollection.getEnclosedFeature getWkbGeom Geometry.Point getGeoPoint)
  pure $ Geospatial.MultiPoint $ Geospatial.mergeGeoPoints geoPoints

getGeoPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPoint
getGeoPoint endianType coordType = do
  point <- getCoordPoint endianType coordType
  pure $ Geospatial.GeoPoint point

getCoordPoints :: Endian.EndianType -> Geometry.CoordinateType -> Word.Word32 -> BinaryGet.Get (Vector.Vector Geospatial.GeoPositionWithoutCRS)
getCoordPoints endianType coordType numberOfPoints =
  Vector.generateM (fromIntegral numberOfPoints) (const $ getCoordPoint endianType coordType)

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
