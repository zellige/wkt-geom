module Data.Wkb.Point where

import           Control.Monad     ((>>=))
import qualified Control.Monad     as Monad
import qualified Data.Binary.Get   as BinaryGet
import qualified Data.Geospatial   as Geospatial
import qualified Data.Int          as Int

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Geometry as Geometry

getPoint :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getPoint endianType coordType = do
  geoPoint <- getGeoPoint endianType coordType
  pure $ Geospatial.Point geoPoint

getMultiPoint :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getMultiPoint endianType coordType =
  Endian.getFourBytes endianType >>= (getPoints endianType coordType)

getPoints :: Endian.EndianType -> Geometry.WkbCoordinateType -> Int.Int32 -> BinaryGet.Get Geospatial.GeospatialGeometry
getPoints endianType coordType numberOfPoints = do
  geoPoints <- Monad.forM [1..numberOfPoints] (\_ -> getGeoPoint endianType coordType)
  pure $ Geospatial.MultiPoint $ Geospatial.mergeGeoPoints geoPoints

getGeoPoint :: Endian.EndianType -> Geometry.WkbCoordinateType -> BinaryGet.Get Geospatial.GeoPoint
getGeoPoint endianType _ = do
  x <- Endian.getDouble endianType
  y <- Endian.getDouble endianType
  pure $ Geospatial.GeoPoint [x,y]
