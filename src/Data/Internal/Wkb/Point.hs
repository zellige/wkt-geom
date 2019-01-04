module Data.Internal.Wkb.Point
  ( getPoint
  , getMultiPoint
  , getGeoPoint
  , getCoordPoint
  , getCoordPoints
  , builderPoint
  , builderMultiPoint
  , builderCoordPoint
  , builderCoordPoints
  ) where

import qualified Data.Binary.Get                      as BinaryGet
import qualified Data.ByteString.Builder              as ByteStringBuilder
import qualified Data.Foldable                        as Foldable
import qualified Data.Geospatial                      as Geospatial
import           Data.Monoid                          ((<>))
import qualified Data.Monoid                          as Monoid
import qualified Data.Sequence                        as Sequence
import qualified Data.Word                            as Word

import qualified Data.Internal.Wkb.Endian             as Endian
import qualified Data.Internal.Wkb.Geometry           as Geometry
import qualified Data.Internal.Wkb.GeometryCollection as GeometryCollection

-- Binary parsers

getPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getPoint endianType coordType = do
  geoPoint <- getGeoPoint endianType coordType
  pure $ Geospatial.Point geoPoint

getMultiPoint :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType) -> Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getMultiPoint getWkbGeom endianType _ = do
  numberOfPoints <- Endian.getFourBytes endianType
  geoPoints <- Sequence.replicateM (fromIntegral numberOfPoints) (GeometryCollection.getEnclosedFeature getWkbGeom Geometry.Point getGeoPoint)
  pure $ Geospatial.MultiPoint $ Geospatial.mergeGeoPoints geoPoints

getGeoPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPoint
getGeoPoint endianType coordType = do
  p <- getCoordPoint endianType coordType
  pure $ Geospatial.GeoPoint p

getCoordPoint :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoPositionWithoutCRS
getCoordPoint endianType coordType =
  case coordType of
    Geometry.TwoD -> do
      point <- Geospatial.PointXY <$> getDouble <*> getDouble
      return $ Geospatial.GeoPointXY point
    Geometry.Z -> do
      point <- Geospatial.PointXYZ <$> getDouble <*> getDouble <*> getDouble
      return $ Geospatial.GeoPointXYZ point
    Geometry.M -> do
      point <- Geospatial.PointXYZ <$> getDouble <*> getDouble <*> getDouble
      return $ Geospatial.GeoPointXYZ point
    Geometry.ZM -> do
      point <- Geospatial.PointXYZM <$> getDouble <*> getDouble <*> getDouble <*> getDouble
      return $ Geospatial.GeoPointXYZM point
  where getDouble = Endian.getDouble endianType

getCoordPoints :: Endian.EndianType -> Geometry.CoordinateType -> Word.Word32 -> BinaryGet.Get (Sequence.Seq Geospatial.GeoPositionWithoutCRS)
getCoordPoints endianType coordType numberOfPoints =
  Sequence.replicateM (fromIntegral numberOfPoints) (getCoordPoint endianType coordType)


-- Binary builders

builderPoint :: Geometry.BuilderWkbGeometryType -> Endian.EndianType -> Geospatial.GeoPoint -> ByteStringBuilder.Builder
builderPoint builderWkbGeom endianType (Geospatial.GeoPoint coordPoint) =
  case Geometry.geoPositionWithoutCRSToCoordinateType coordPoint of
    Just coordinateType ->
      Endian.builderEndianType endianType
        <> builderWkbGeom endianType (Geometry.WkbGeom Geometry.Point coordinateType)
        <> builderCoordPoint endianType coordPoint
    Nothing ->
      Monoid.mempty

builderMultiPoint :: Geometry.BuilderWkbGeometryType -> Endian.EndianType -> Geospatial.GeoMultiPoint -> ByteStringBuilder.Builder
builderMultiPoint builderWkbGeom endianType (Geospatial.GeoMultiPoint coordPoints) =
  Endian.builderEndianType endianType
    <> builderWkbGeom endianType (Geometry.WkbGeom Geometry.MultiPoint coordType)
    <> Endian.builderFourBytes endianType (fromIntegral $ length coordPoints)
    <> Foldable.foldMap (builderPoint builderWkbGeom endianType . Geospatial.GeoPoint) coordPoints
  where coordType = Geometry.coordTypeOfSequence coordPoints

builderCoordPoint :: Endian.EndianType -> Geospatial.GeoPositionWithoutCRS -> ByteStringBuilder.Builder
builderCoordPoint endianType coordPoint =
  case coordPoint of
    Geospatial.GeoEmpty -> Monoid.mempty
    Geospatial.GeoPointXY (Geospatial.PointXY x y) ->
      Foldable.foldMap builderDouble [x, y]
    Geospatial.GeoPointXYZ (Geospatial.PointXYZ x y z) ->
      Foldable.foldMap builderDouble [x, y, z]
    Geospatial.GeoPointXYZM (Geospatial.PointXYZM x y z m) ->
      Foldable.foldMap builderDouble [x, y, z, m]
  where builderDouble = Endian.builderDouble endianType

builderCoordPoints :: Endian.EndianType -> Sequence.Seq Geospatial.GeoPositionWithoutCRS -> ByteStringBuilder.Builder
builderCoordPoints endianType =
  Foldable.foldMap (builderCoordPoint endianType)
