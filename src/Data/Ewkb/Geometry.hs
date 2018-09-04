module Data.Ewkb.Geometry where

import qualified Control.Monad     as Monad
import qualified Data.Binary.Get   as BinaryGet
import           Data.Bits         ((.&.))
import qualified Data.Word         as Word

import qualified Data.Wkb.Endian   as Endian
import qualified Data.Wkb.Geometry as Geometry

data EwkbSrid = Srid Word.Word32 | NoSrid deriving (Show, Eq)

data EwkbGeometryType = EwkbGeom Geometry.WkbGeometryTypeWithCoords EwkbSrid deriving (Show, Eq)

getEwkbGeometryType :: Endian.EndianType -> BinaryGet.Get EwkbGeometryType
getEwkbGeometryType endianType = do
  rawGeometryType <- Endian.getFourBytes endianType
  ewkbSrid <- getEwkbSrid endianType rawGeometryType
  wkbGeometryType <- rawtoWkbGeometryType rawGeometryType
  pure $ EwkbGeom wkbGeometryType ewkbSrid

getWkbGeometryType :: Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryTypeWithCoords
getWkbGeometryType endianType = do
  rawGeometryType <- Endian.getFourBytes endianType
  rawtoWkbGeometryType rawGeometryType

rawtoWkbGeometryType :: Word.Word32 -> BinaryGet.Get Geometry.WkbGeometryTypeWithCoords
rawtoWkbGeometryType rawGeometryType = do
  let geomType = intToGeometryType rawGeometryType
      coordType = intToCoordinateType rawGeometryType
  case geomType of
    Just g -> pure $ Geometry.WkbGeom g coordType
    _      -> Monad.fail $ "Invalid EwkbGeometry: " ++ show rawGeometryType

getEwkbSrid :: Endian.EndianType -> Word.Word32 -> BinaryGet.Get EwkbSrid
getEwkbSrid endianType int =
  if int .&. 0x20000000 /= 0 then do
    srid <- Endian.getFourBytes endianType
    pure $ Srid srid
  else
    pure NoSrid

intToGeometryType :: Word.Word32 -> Maybe Geometry.WkbGeometryType
intToGeometryType int =
  case int .&. 0x0fffffff of
    0 -> Just Geometry.WkbGeometry
    1 -> Just Geometry.WkbPoint
    2 -> Just Geometry.WkbLineString
    3 -> Just Geometry.WkbPolygon
    4 -> Just Geometry.WkbMultiPoint
    5 -> Just Geometry.WkbMultiLineString
    6 -> Just Geometry.WkbMultiPolygon
    7 -> Just Geometry.WkbGeometryCollection
    _ -> Nothing

intToCoordinateType :: Word.Word32 -> Geometry.WkbCoordinateType
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

