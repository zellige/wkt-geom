module Data.Wkb.Point where

import qualified Control.Monad     as Monad
import qualified Data.Binary.Get   as BinaryGet
import qualified Data.Geospatial   as Geospatial
import qualified Data.Word         as Word

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Feature  as Feature
import qualified Data.Wkb.Geometry as Geometry

getPoint :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getPoint endianType coordType = do
  geoPoint <- getGeoPoint endianType coordType
  pure $ Geospatial.Point geoPoint

getMultiPoint :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryTypeWithCoords) -> Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getMultiPoint getWkbGeom endianType _ = do
  numberOfPoints <- Endian.getFourBytes endianType
  geoPoints <- Monad.forM [1..numberOfPoints] (const $ Feature.getEnclosedFeature getGeoPoint getWkbGeom Geometry.WkbPoint)
  pure $ Geospatial.MultiPoint $ Geospatial.mergeGeoPoints geoPoints

getGeoPoint :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeoPoint
getGeoPoint endianType coordType = do
  point <- getCoordPoint endianType coordType
  pure $ Geospatial.GeoPoint point

getCoordPoints :: Endian.EndianType -> Geometry.WkbCoordinateType -> Word.Word32 -> BinaryGet.Get [[Double]]
getCoordPoints endianType coordType numberOfPoints =
  Monad.forM [1..numberOfPoints] (\_ -> getCoordPoint endianType coordType)

getCoordPoint :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get [Double]
getCoordPoint endianType coordType =
  case coordType of
    Geometry.TwoD -> do
      x <- Endian.getDouble endianType
      y <- Endian.getDouble endianType
      pure [x,y]
    Geometry.Z -> do
      x <- Endian.getDouble endianType
      y <- Endian.getDouble endianType
      z <- Endian.getDouble endianType
      pure [x,y,z]
    Geometry.M -> do
      x <- Endian.getDouble endianType
      y <- Endian.getDouble endianType
      m <- Endian.getDouble endianType
      pure [x,y,m]
    Geometry.ZM -> do
      x <- Endian.getDouble endianType
      y <- Endian.getDouble endianType
      z <- Endian.getDouble endianType
      m <- Endian.getDouble endianType
      pure [x,y,z,m]
