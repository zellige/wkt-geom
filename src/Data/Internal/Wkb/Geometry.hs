module Data.Internal.Wkb.Geometry
  ( GeometryType (..)
  , CoordinateType (..)
  , WkbGeometryType (..)
  , getGeometryTypeWithCoords
  , builderGeometryType
  , geoPositionWithoutCRSToCoordinateType
  , coordTypeOfSequence
  , coordTypeOfLinearRings
  ) where

import qualified Control.Monad            as Monad
import qualified Data.Binary.Get          as BinaryGet
import qualified Data.ByteString.Builder  as ByteStringBuilder
import qualified Data.Geospatial          as Geospatial
import qualified Data.LinearRing          as LinearRing
import qualified Data.Maybe               as Maybe
import qualified Data.Sequence            as Sequence
import qualified Data.Word                as Word

import qualified Data.Internal.Wkb.Endian as Endian

data GeometryType
  = Geometry
  | Point
  | LineString
  | Polygon
  | MultiPoint
  | MultiLineString
  | MultiPolygon
  | GeometryCollection deriving (Show, Eq)

data CoordinateType = TwoD | Z | M | ZM  deriving (Show, Eq)

data WkbGeometryType = WkbGeom GeometryType CoordinateType deriving (Show, Eq)


-- Binary parsers

getGeometryTypeWithCoords :: Endian.EndianType -> BinaryGet.Get WkbGeometryType
getGeometryTypeWithCoords endianType = do
  fullGeometryType <- Endian.getFourBytes endianType
  let geomType = intToGeometryType $ fullGeometryType `rem` 1000
      coordType = intToCoordinateType $ fullGeometryType `div` 1000
  case (geomType, coordType) of
    (Just g, Just c) -> pure $ WkbGeom g c
    _                ->
      Monad.fail $ "Invalid WkbGeometryType: " ++ show fullGeometryType


-- Binary builders

builderGeometryType :: Endian.EndianType -> WkbGeometryType -> ByteStringBuilder.Builder
builderGeometryType endianType (WkbGeom geometryType coordinateType) = do
  let int = coordinateTypeToInt coordinateType * 1000 + geometryTypeToInt geometryType
  Endian.builderFourBytes endianType int


-- Helpers

geoPositionWithoutCRSToCoordinateType :: Geospatial.GeoPositionWithoutCRS -> Maybe CoordinateType
geoPositionWithoutCRSToCoordinateType geoPosition =
  case geoPosition of
    Geospatial.GeoEmpty       -> Nothing
    Geospatial.GeoPointXY _   -> Just TwoD
    Geospatial.GeoPointXYZ _  -> Just Z
    Geospatial.GeoPointXYZM _ -> Just ZM

coordTypeOfSequence :: Sequence.Seq Geospatial.GeoPositionWithoutCRS -> CoordinateType
coordTypeOfSequence (first Sequence.:<| _) =
  Maybe.fromMaybe TwoD (geoPositionWithoutCRSToCoordinateType first)
coordTypeOfSequence _ = TwoD

coordTypeOfLinearRings :: Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS) -> CoordinateType
coordTypeOfLinearRings (first Sequence.:<| _) = coordTypeOfSequence $ LinearRing.toSeq first
coordTypeOfLinearRings _ = TwoD

intToGeometryType :: Word.Word32 -> Maybe GeometryType
intToGeometryType int =
  case int of
    0 -> Just Geometry
    1 -> Just Point
    2 -> Just LineString
    3 -> Just Polygon
    4 -> Just MultiPoint
    5 -> Just MultiLineString
    6 -> Just MultiPolygon
    7 -> Just GeometryCollection
    _ -> Nothing

geometryTypeToInt :: GeometryType -> Word.Word32
geometryTypeToInt geometryType =
  case geometryType of
    Geometry           -> 0
    Point              -> 1
    LineString         -> 2
    Polygon            -> 3
    MultiPoint         -> 4
    MultiLineString    -> 5
    MultiPolygon       -> 6
    GeometryCollection -> 7

intToCoordinateType :: Word.Word32 -> Maybe CoordinateType
intToCoordinateType int =
  case int of
    0 -> Just TwoD
    1 -> Just Z
    2 -> Just M
    3 -> Just ZM
    _ -> Nothing

coordinateTypeToInt :: CoordinateType -> Word.Word32
coordinateTypeToInt coordinateType =
  case coordinateType of
    TwoD -> 0
    Z    -> 1
    M    -> 2
    ZM   -> 3
