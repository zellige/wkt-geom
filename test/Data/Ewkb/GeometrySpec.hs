{-# LANGUAGE OverloadedStrings #-}

module Data.Ewkb.GeometrySpec where

import qualified Data.Binary.Get         as BinaryGet
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy    as LazyByteString
import           Data.Monoid             ((<>))
import qualified Data.Word               as Word
import           Test.Hspec              (Spec, describe, it, shouldBe)

import qualified Data.Ewkb.Geometry      as Ewkb
import qualified Data.Wkb.Endian         as Endian
import qualified Data.Wkb.Geometry       as Wkb

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
            BinaryGet.runGet (Ewkb.getEwkbGeometryType endianType) (getByteString endianType  rawGeomType maybeSrid) `shouldBe` expected

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
  [ (0x00000000, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometry Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000001, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPoint Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000002, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbLineString Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000003, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPolygon Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000004, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPoint Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000005, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiLineString Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000006, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPolygon Wkb.TwoD) Ewkb.NoSrid)
  , (0x00000007, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometryCollection Wkb.TwoD) Ewkb.NoSrid)
  , (0x80000000, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometry Wkb.Z) Ewkb.NoSrid)
  , (0x80000001, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPoint Wkb.Z) Ewkb.NoSrid)
  , (0x80000002, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbLineString Wkb.Z) Ewkb.NoSrid)
  , (0x80000003, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPolygon Wkb.Z) Ewkb.NoSrid)
  , (0x80000004, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPoint Wkb.Z) Ewkb.NoSrid)
  , (0x80000005, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiLineString Wkb.Z) Ewkb.NoSrid)
  , (0x80000006, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPolygon Wkb.Z) Ewkb.NoSrid)
  , (0x80000007, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometryCollection Wkb.Z) Ewkb.NoSrid)
  , (0x40000000, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometry Wkb.M) Ewkb.NoSrid)
  , (0x40000001, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPoint Wkb.M) Ewkb.NoSrid)
  , (0x40000002, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbLineString Wkb.M) Ewkb.NoSrid)
  , (0x40000003, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPolygon Wkb.M) Ewkb.NoSrid)
  , (0x40000004, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPoint Wkb.M) Ewkb.NoSrid)
  , (0x40000005, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiLineString Wkb.M) Ewkb.NoSrid)
  , (0x40000006, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPolygon Wkb.M) Ewkb.NoSrid)
  , (0x40000007, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometryCollection Wkb.M) Ewkb.NoSrid)
  , (0xC0000000, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometry Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000001, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPoint Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000002, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbLineString Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000003, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPolygon Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000004, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPoint Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000005, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiLineString Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000006, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPolygon Wkb.ZM) Ewkb.NoSrid)
  , (0xC0000007, Nothing, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometryCollection Wkb.ZM) Ewkb.NoSrid)
  , (0x20000000, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometry Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000001, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPoint Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000002, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbLineString Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000003, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPolygon Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000004, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPoint Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000005, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiLineString Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000006, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPolygon Wkb.TwoD) (Ewkb.Srid srid))
  , (0x20000007, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometryCollection Wkb.TwoD) (Ewkb.Srid srid))
  , (0xA0000000, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometry Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000001, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPoint Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000002, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbLineString Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000003, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPolygon Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000004, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPoint Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000005, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiLineString Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000006, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPolygon Wkb.Z) (Ewkb.Srid srid))
  , (0xA0000007, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometryCollection Wkb.Z) (Ewkb.Srid srid))
  , (0x60000000, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometry Wkb.M) (Ewkb.Srid srid))
  , (0x60000001, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPoint Wkb.M) (Ewkb.Srid srid))
  , (0x60000002, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbLineString Wkb.M) (Ewkb.Srid srid))
  , (0x60000003, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPolygon Wkb.M) (Ewkb.Srid srid))
  , (0x60000004, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPoint Wkb.M) (Ewkb.Srid srid))
  , (0x60000005, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiLineString Wkb.M) (Ewkb.Srid srid))
  , (0x60000006, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPolygon Wkb.M) (Ewkb.Srid srid))
  , (0x60000007, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometryCollection Wkb.M) (Ewkb.Srid srid))
  , (0xE0000000, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometry Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000001, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPoint Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000002, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbLineString Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000003, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbPolygon Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000004, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPoint Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000005, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiLineString Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000006, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbMultiPolygon Wkb.ZM) (Ewkb.Srid srid))
  , (0xE0000007, Just srid, Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.WkbGeometryCollection Wkb.ZM) (Ewkb.Srid srid))
  ]

srid :: Word.Word32
srid = 4326
