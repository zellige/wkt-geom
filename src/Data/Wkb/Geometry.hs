module Data.Wkb.Geometry where

import qualified Control.Monad   as Monad
import qualified Data.Binary.Get as BinaryGet
import qualified GHC.Int         as Int

import qualified Data.Wkb.Endian as Endian

data WkbGeometryType
  = WkbGeometry
  | WkbPoint
  | WkbLineString
  | WkbPolygon
  | WkbMultiPoint
  | WkbMultiLineString
  | WkbMultiPolygon
  | WkbGeometryCollection deriving (Show, Eq)

data WkbCoordinateType = TwoD | Z | M | ZM  deriving (Show, Eq)

data WkbGeometryTypeWithCoords = WkbGeom WkbGeometryType WkbCoordinateType deriving (Show, Eq)

getGeometryTypeWithCoords :: Endian.EndianType -> BinaryGet.Get WkbGeometryTypeWithCoords
getGeometryTypeWithCoords endianType = do
  fullGeometryType <- Endian.getFourBytes endianType
  geomType <- pure $ intToGeometryType $ fullGeometryType `rem` 1000
  coordType <- pure $ intToCoordinateType $ fullGeometryType `div` 1000
  case (geomType, coordType) of
    (Just g, Just c) -> pure $ WkbGeom g c
    _                -> Monad.fail "Invalid WkbGeometryTypeWithCoords"

intToGeometryType :: Int.Int32 -> Maybe WkbGeometryType
intToGeometryType int =
  case int of
    0 -> Just WkbGeometry
    1 -> Just WkbPoint
    2 -> Just WkbLineString
    3 -> Just WkbPolygon
    4 -> Just WkbMultiPoint
    5 -> Just WkbMultiLineString
    6 -> Just WkbMultiPolygon
    7 -> Just WkbGeometryCollection
    _ -> Nothing

intToCoordinateType :: Int.Int32 -> Maybe WkbCoordinateType
intToCoordinateType int =
  case int of
    0 -> Just TwoD
    1 -> Just Z
    2 -> Just M
    3 -> Just ZM
    _ -> Nothing
