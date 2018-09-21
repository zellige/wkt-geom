{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Ewkb.GeometrySpec where

import qualified Data.Binary.Get             as BinaryGet
import qualified Data.ByteString.Builder     as ByteStringBuilder
import qualified Data.ByteString.Lazy        as LazyByteString
import           Data.Monoid                 ((<>))
import qualified Data.Word                   as Word
import           Test.Hspec                  (Spec, describe, it, shouldBe)

import qualified Data.Internal.Ewkb.Geometry as Ewkb
import qualified Data.Internal.Wkb.Endian    as Endian
import qualified Data.Internal.Wkb.Geometry  as Wkb

spec :: Spec
spec =
  testValidGetEwkbGeometryType

testValidGetEwkbGeometryType :: Spec
testValidGetEwkbGeometryType =
  describe "get extended wkb geometry type for valid data" $
    mapM_ testGetEwkbGeometryType expectations

testGetEwkbGeometryType :: (Word.Word32, Maybe Word.Word32, Ewkb.EwkbGeometryType) -> Spec
testGetEwkbGeometryType (rawGeomType, maybeSrid, expected) =
  it ("Parse " ++ show expected) $
    mapM_ test [Endian.BigEndian, Endian.LittleEndian]
    where test endianType =
            BinaryGet.runGet (Ewkb.ewkbGeometryType endianType) (getByteString endianType  rawGeomType maybeSrid) `shouldBe` expected

getByteString :: Endian.EndianType -> Word.Word32 -> Maybe Word.Word32 -> LazyByteString.ByteString
getByteString endianType rawGeomType maybeSrid =
  ByteStringBuilder.toLazyByteString $
    case maybeSrid of
      Just srid' ->
        case endianType of
          Endian.LittleEndian ->
            ByteStringBuilder.word32LE rawGeomType <> ByteStringBuilder.word32LE srid'
          Endian.BigEndian ->
            ByteStringBuilder.word32BE rawGeomType <> ByteStringBuilder.word32BE srid'
      Nothing ->
        case endianType of
          Endian.LittleEndian ->
            ByteStringBuilder.word32LE rawGeomType
          Endian.BigEndian ->
            ByteStringBuilder.word32BE rawGeomType

expectations :: [(Word.Word32, Maybe Word.Word32,  Ewkb.EwkbGeometryType)]
expectations =
  [ (0x00000000, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000001, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000002, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000003, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000004, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000005, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000006, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000007, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.TwoD) Ewkb.NoSrid)
  , (0x80000000, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.Z) Ewkb.NoSrid)
  , (0x80000001, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.Z) Ewkb.NoSrid)
  , (0x80000002, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.Z) Ewkb.NoSrid)
  , (0x80000003, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.Z) Ewkb.NoSrid)
  , (0x80000004, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.Z) Ewkb.NoSrid)
  , (0x80000005, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.Z) Ewkb.NoSrid)
  , (0x80000006, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.Z) Ewkb.NoSrid)
  , (0x80000007, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.Z) Ewkb.NoSrid)
  , (0x40000000, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.M) Ewkb.NoSrid)
  , (0x40000001, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.M) Ewkb.NoSrid)
  , (0x40000002, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.M) Ewkb.NoSrid)
  , (0x40000003, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.M) Ewkb.NoSrid)
  , (0x40000004, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.M) Ewkb.NoSrid)
  , (0x40000005, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.M) Ewkb.NoSrid)
  , (0x40000006, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.M) Ewkb.NoSrid)
  , (0x40000007, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.M) Ewkb.NoSrid)
  , (0xC0000000, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000001, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000002, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000003, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000004, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000005, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000006, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000007, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.ZM) Ewkb.NoSrid)
  , (0x20000000, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000001, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000002, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000003, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000004, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000005, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000006, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000007, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.TwoD) (Ewkb.Srid srid))
  , (0xA0000000, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000001, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000002, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000003, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000004, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000005, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000006, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000007, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.Z) (Ewkb.Srid srid))
  , (0x60000000, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.M) (Ewkb.Srid srid))
  , (0x60000001, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.M) (Ewkb.Srid srid))
  , (0x60000002, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.M) (Ewkb.Srid srid))
  , (0x60000003, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.M) (Ewkb.Srid srid))
  , (0x60000004, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.M) (Ewkb.Srid srid))
  , (0x60000005, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.M) (Ewkb.Srid srid))
  , (0x60000006, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.M) (Ewkb.Srid srid))
  , (0x60000007, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.M) (Ewkb.Srid srid))
  , (0xE0000000, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000001, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000002, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000003, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000004, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000005, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000006, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000007, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.ZM) (Ewkb.Srid srid))
  ]

srid :: Word.Word32
srid = 4326
