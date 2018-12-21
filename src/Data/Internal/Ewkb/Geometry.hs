module Data.Internal.Ewkb.Geometry
  ( EwkbGeometryType (..)
  , SridType (..)
  , ewkbGeometryType
  , wkbGeometryType
  ) where

import qualified Control.Monad              as Monad
import qualified Data.Binary.Get            as BinaryGet
import           Data.Bits                  ((.&.))
import qualified Data.Word                  as Word

import qualified Data.Internal.Wkb.Endian   as Endian
import qualified Data.Internal.Wkb.Geometry as Geometry

data SridType = Srid Word.Word32 | NoSrid deriving (Show, Eq)

data EwkbGeometryType = EwkbGeom Geometry.WkbGeometryType SridType deriving (Show, Eq)

ewkbGeometryType :: Endian.EndianType -> BinaryGet.Get EwkbGeometryType
ewkbGeometryType endianType = do
  rawGeometryType <- Endian.getFourBytes endianType
  ewkbSrid <- getEwkbSrid endianType rawGeometryType
  geomType <- rawtoWkbGeometryType rawGeometryType
  pure $ EwkbGeom geomType ewkbSrid

wkbGeometryType :: Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType
wkbGeometryType endianType = do
  rawGeometryType <- Endian.getFourBytes endianType
  _ <- getEwkbSrid endianType rawGeometryType
  rawtoWkbGeometryType rawGeometryType

rawtoWkbGeometryType :: Word.Word32 -> BinaryGet.Get Geometry.WkbGeometryType
rawtoWkbGeometryType rawGeometryType = do
  let geomType = intToGeometryType rawGeometryType
      coordType = intToCoordinateType rawGeometryType
  case geomType of
    Just g -> pure $ Geometry.WkbGeom g coordType
    _      -> Monad.fail $ "Invalid EwkbGeometry: " ++ show rawGeometryType

getEwkbSrid :: Endian.EndianType -> Word.Word32 -> BinaryGet.Get SridType
getEwkbSrid endianType int =
  if int .&. 0x20000000 /= 0 then do
    srid <- Endian.getFourBytes endianType
    if srid == 4326 then
      pure $ Srid srid
    else
      Monad.fail $ "Invalid SRID only 4326 supported: " ++ show srid
  else
    pure NoSrid

intToGeometryType :: Word.Word32 -> Maybe Geometry.GeometryType
intToGeometryType int =
  case int .&. 0x0fffffff of
    0 -> Just Geometry.Geometry
    1 -> Just Geometry.Point
    2 -> Just Geometry.LineString
    3 -> Just Geometry.Polygon
    4 -> Just Geometry.MultiPoint
    5 -> Just Geometry.MultiLineString
    6 -> Just Geometry.MultiPolygon
    7 -> Just Geometry.GeometryCollection
    _ -> Nothing

intToCoordinateType :: Word.Word32 -> Geometry.CoordinateType
intToCoordinateType int =
  case (hasZ int, hasM int) of
    (False, False) -> Geometry.TwoD
    (False, True)  -> Geometry.M
    (True, False)  -> Geometry.Z
    (True, True)   -> Geometry.ZM

hasZ :: Word.Word32 -> Bool
hasZ int =
  int .&. 0x80000000 /= 0

hasM :: Word.Word32 -> Bool
hasM int =
  int .&. 0x40000000 /= 0

