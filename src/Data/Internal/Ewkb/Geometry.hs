module Data.Internal.Ewkb.Geometry
  ( EwkbGeometryType (..)
  , SridType (..)
  , getEwkbGeom
  , getWkbGeom
  , builderWkbGeom
  , builderEwkbGeom
  ) where

import qualified Control.Monad              as Monad
import qualified Data.Binary.Get            as BinaryGet
import           Data.Bits                  ((.&.), (.|.))
import qualified Data.ByteString.Builder    as ByteStringBuilder
import           Data.Monoid                ((<>))
import qualified Data.Word                  as Word

import qualified Data.Internal.Wkb.Endian   as Endian
import qualified Data.Internal.Wkb.Geometry as Geometry

data SridType = Srid Word.Word32 | NoSrid deriving (Show, Eq)

data EwkbGeometryType = EwkbGeom Geometry.WkbGeometryType SridType deriving (Show, Eq)


-- Binary parsers

getEwkbGeom :: Endian.EndianType -> BinaryGet.Get EwkbGeometryType
getEwkbGeom endianType = do
  rawGeometryType <- Endian.getFourBytes endianType
  ewkbSrid <- getEwkbSrid endianType rawGeometryType
  geomType <- rawtoWkbGeometryType rawGeometryType
  pure $ EwkbGeom geomType ewkbSrid

getWkbGeom :: Endian.EndianType -> BinaryGet.Get Geometry.WkbGeometryType
getWkbGeom endianType = do
  rawGeometryType <- Endian.getFourBytes endianType
  _ <- getEwkbSrid endianType rawGeometryType
  rawtoWkbGeometryType rawGeometryType


-- Binary builders

builderEwkbGeom :: Endian.EndianType -> EwkbGeometryType -> ByteStringBuilder.Builder
builderEwkbGeom endianType (EwkbGeom wkbGeometryType NoSrid) =
  builderWkbGeom endianType wkbGeometryType
builderEwkbGeom endianType (EwkbGeom wkbGeometryType (Srid srid)) = do
  let int = wkbGeometryTypeToInt wkbGeometryType .|. sridMask
  Endian.builderFourBytes endianType int
    <> Endian.builderFourBytes endianType srid

builderWkbGeom :: Endian.EndianType -> Geometry.WkbGeometryType -> ByteStringBuilder.Builder
builderWkbGeom endianType wkbGeometryType =
  Endian.builderFourBytes endianType $ wkbGeometryTypeToInt wkbGeometryType


-- Helpers

wkbGeometryTypeToInt :: Geometry.WkbGeometryType -> Word.Word32
wkbGeometryTypeToInt (Geometry.WkbGeom geometryType coordinateType) =
  coordinateTypeToInt coordinateType .|. geometryTypeToInt geometryType

rawtoWkbGeometryType :: Word.Word32 -> BinaryGet.Get Geometry.WkbGeometryType
rawtoWkbGeometryType rawGeometryType = do
  let geomType = intToGeometryType rawGeometryType
      coordType = intToCoordinateType rawGeometryType
  case geomType of
    Just g -> pure $ Geometry.WkbGeom g coordType
    _      -> Monad.fail $ "Invalid EwkbGeometry: " ++ show rawGeometryType

getEwkbSrid :: Endian.EndianType -> Word.Word32 -> BinaryGet.Get SridType
getEwkbSrid endianType int =
  if int .&. sridMask /= 0 then do
    srid <- Endian.getFourBytes endianType
    if srid == supportedSrid then
      pure $ Srid srid
    else
      Monad.fail $ "Invalid SRID only " <> show supportedSrid <> " supported: " ++ show srid
  else
    pure NoSrid

intToGeometryType :: Word.Word32 -> Maybe Geometry.GeometryType
intToGeometryType int =
  case int .&. geometryMask of
    0 -> Just Geometry.Geometry
    1 -> Just Geometry.Point
    2 -> Just Geometry.LineString
    3 -> Just Geometry.Polygon
    4 -> Just Geometry.MultiPoint
    5 -> Just Geometry.MultiLineString
    6 -> Just Geometry.MultiPolygon
    7 -> Just Geometry.GeometryCollection
    _ -> Nothing

geometryTypeToInt :: Geometry.GeometryType -> Word.Word32
geometryTypeToInt geometryType =
  case geometryType of
    Geometry.Geometry           -> 0
    Geometry.Point              -> 1
    Geometry.LineString         -> 2
    Geometry.Polygon            -> 3
    Geometry.MultiPoint         -> 4
    Geometry.MultiLineString    -> 5
    Geometry.MultiPolygon       -> 6
    Geometry.GeometryCollection -> 7

intToCoordinateType :: Word.Word32 -> Geometry.CoordinateType
intToCoordinateType int =
  case (hasZ int, hasM int) of
    (False, False) -> Geometry.TwoD
    (False, True)  -> Geometry.M
    (True, False)  -> Geometry.Z
    (True, True)   -> Geometry.ZM

coordinateTypeToInt :: Geometry.CoordinateType -> Word.Word32
coordinateTypeToInt coordinateType =
  case coordinateType of
    Geometry.TwoD -> 0
    Geometry.Z    -> zMask
    Geometry.M    -> mMask
    Geometry.ZM   -> zMask .|. mMask

hasZ :: Word.Word32 -> Bool
hasZ int =
  int .&. zMask /= 0

hasM :: Word.Word32 -> Bool
hasM int =
  int .&. 0x40000000 /= 0


-- Constants

zMask :: Word.Word32
zMask = 0x80000000

mMask :: Word.Word32
mMask = 0x40000000

sridMask :: Word.Word32
sridMask = 0x20000000

geometryMask :: Word.Word32
geometryMask = 0x0fffffff

supportedSrid :: Word.Word32
supportedSrid = 4326
