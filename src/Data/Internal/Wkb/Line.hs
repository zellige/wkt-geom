module Data.Internal.Wkb.Line
  ( Data.Internal.Wkb.Line.getLine
  , getMultiLine
  , builderLine
  , builderMultiLine
  ) where

import qualified Control.Monad                        as Monad
import qualified Data.Binary.Get                      as BinaryGet
import qualified Data.ByteString.Builder              as ByteStringBuilder
import qualified Data.Foldable                        as Foldable
import qualified Data.Geospatial                      as Geospatial
import qualified Data.LineString                      as LineString
import           Data.Monoid                          ((<>))
import qualified Data.Sequence                        as Sequence


import qualified Data.Internal.Wkb.Endian             as Endian
import qualified Data.Internal.Wkb.Geometry           as Geometry
import qualified Data.Internal.Wkb.GeometryCollection as GeometryCollection
import qualified Data.Internal.Wkb.Point              as Point

-- Binary parsers

getLine :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getLine endianType coordType = do
  gl <- getGeoLine endianType coordType
  pure $ Geospatial.Line gl

getMultiLine :: (Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType) -> Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeospatialGeometry
getMultiLine getWkbGeom endianType _ = do
  numberOfLines <- Endian.getFourBytes endianType
  geoLines <- Sequence.replicateM (fromIntegral numberOfLines) (GeometryCollection.getEnclosedFeature getWkbGeom Geometry.LineString getGeoLine)
  pure $ Geospatial.MultiLine $ Geospatial.mergeGeoLines geoLines

getGeoLine :: Endian.EndianType -> Geometry.CoordinateType -> BinaryGet.Get Geospatial.GeoLine
getGeoLine endianType coordType = do
  numberOfPoints <- Endian.getFourBytes endianType
  if numberOfPoints >= 2 then do
    p1 <- Point.getCoordPoint endianType coordType
    p2 <- Point.getCoordPoint endianType coordType
    pts <- Point.getCoordPoints endianType coordType (numberOfPoints - 2)
    pure $ Geospatial.GeoLine $ LineString.makeLineString p1 p2 pts
  else
    Monad.fail "Must have at least two points for a line"


-- Binary builders

builderLine :: Geometry.BuilderWkbGeometryType -> Endian.EndianType -> Geospatial.GeoLine -> ByteStringBuilder.Builder
builderLine builderWkbGeom endianType (Geospatial.GeoLine lineString) = do
  let coordPoints = LineString.toSeq lineString
      coordType = Geometry.coordTypeOfSequence coordPoints
  Endian.builderEndianType endianType
    <> builderWkbGeom endianType (Geometry.WkbGeom Geometry.LineString coordType)
    <> Endian.builderFourBytes endianType (fromIntegral $ length coordPoints)
    <> Foldable.foldMap (Point.builderCoordPoint endianType) coordPoints

builderMultiLine :: Geometry.BuilderWkbGeometryType -> Endian.EndianType -> Geospatial.GeoMultiLine -> ByteStringBuilder.Builder
builderMultiLine builderWkbGeom endianType (Geospatial.GeoMultiLine lineStrings) =
  Endian.builderEndianType endianType
    <> builderWkbGeom endianType (Geometry.WkbGeom Geometry.MultiLineString Geometry.TwoD)
    <> Endian.builderFourBytes endianType (fromIntegral $ length lineStrings)
    <> Foldable.foldMap (builderLine builderWkbGeom endianType . Geospatial.GeoLine) lineStrings
