module Data.Internal.Wkb.Geometry
  ( GeometryType (..)
  , CoordinateType (..)
  , WkbGeometryType (..)
  , getGeometryTypeWithCoords
  , builderGeometryType
  ) where

import qualified Control.Monad            as Monad
import qualified Data.Binary.Get          as BinaryGet
import qualified Data.ByteString.Builder  as ByteStringBuilder
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

getGeometryTypeWithCoords :: Endian.EndianType -> BinaryGet.Get WkbGeometryType
getGeometryTypeWithCoords endianType = do
  fullGeometryType <- Endian.getFourBytes endianType
  let geomType = intToGeometryType $ fullGeometryType `rem` 1000
      coordType = intToCoordinateType $ fullGeometryType `div` 1000
  case (geomType, coordType) of
    (Just g, Just c) -> pure $ WkbGeom g c
    _                ->
      Monad.fail $ "Invalid WkbGeometryType: " ++ show fullGeometryType

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

builderGeometryType :: Endian.EndianType -> WkbGeometryType -> ByteStringBuilder.Builder
builderGeometryType endianType (WkbGeom geometryType coordinateType) = do
  let int = (coordinateTypeToInt coordinateType) * 1000 + (geometryTypeToInt geometryType)
  Endian.builderFourBytes endianType int
