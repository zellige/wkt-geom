{-# LANGUAGE OverloadedStrings #-}

module Data.Wkb.GeometrySpec where

import qualified Data.Binary.Get         as BinaryGet
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.Word               as Word
import           Test.Hspec              (Spec, describe, it, shouldBe)

import qualified Data.Wkb.Endian         as Endian
import qualified Data.Wkb.Geometry       as Geometry

spec :: Spec
spec =
  testValidGetGeometryTypeWithCoords

testValidGetGeometryTypeWithCoords :: Spec
testValidGetGeometryTypeWithCoords =
  describe "get geometry type with coords type for valid data" $
    mapM_ testGetGeometryTypeWithCoords expectations

testGetGeometryTypeWithCoords :: (Word.Word32, Geometry.WkbGeometryType) -> Spec
testGetGeometryTypeWithCoords (int, expected) =
  it ("Parse " ++ show expected) $
    mapM_ test [Endian.BigEndian, Endian.LittleEndian]
    where test endianType =
            BinaryGet.runGet (Geometry.getGeometryTypeWithCoords endianType) (getByteString endianType int) `shouldBe` expected

getByteString :: Endian.EndianType -> Word.Word32 -> LazyByteString.ByteString
getByteString endianType int =
  ByteStringBuilder.toLazyByteString $
    case endianType of
      Endian.LittleEndian ->
        ByteStringBuilder.word32LE int
      Endian.BigEndian ->
        ByteStringBuilder.word32BE int

expectations :: [(Word.Word32, Geometry.WkbGeometryType)]
expectations =
  [ (0000, Geometry.WkbGeom Geometry.Geometry Geometry.TwoD)
  , (0001, Geometry.WkbGeom Geometry.Point Geometry.TwoD)
  , (0002, Geometry.WkbGeom Geometry.LineString Geometry.TwoD)
  , (0003, Geometry.WkbGeom Geometry.Polygon Geometry.TwoD)
  , (0004, Geometry.WkbGeom Geometry.MultiPoint Geometry.TwoD)
  , (0005, Geometry.WkbGeom Geometry.MultiLineString Geometry.TwoD)
  , (0006, Geometry.WkbGeom Geometry.MultiPolygon Geometry.TwoD)
  , (0007, Geometry.WkbGeom Geometry.GeometryCollection Geometry.TwoD)
  , (1000, Geometry.WkbGeom Geometry.Geometry Geometry.Z)
  , (1001, Geometry.WkbGeom Geometry.Point Geometry.Z)
  , (1002, Geometry.WkbGeom Geometry.LineString Geometry.Z)
  , (1003, Geometry.WkbGeom Geometry.Polygon Geometry.Z)
  , (1004, Geometry.WkbGeom Geometry.MultiPoint Geometry.Z)
  , (1005, Geometry.WkbGeom Geometry.MultiLineString Geometry.Z)
  , (1006, Geometry.WkbGeom Geometry.MultiPolygon Geometry.Z)
  , (1007, Geometry.WkbGeom Geometry.GeometryCollection Geometry.Z)
  , (2000, Geometry.WkbGeom Geometry.Geometry Geometry.M)
  , (2001, Geometry.WkbGeom Geometry.Point Geometry.M)
  , (2002, Geometry.WkbGeom Geometry.LineString Geometry.M)
  , (2003, Geometry.WkbGeom Geometry.Polygon Geometry.M)
  , (2004, Geometry.WkbGeom Geometry.MultiPoint Geometry.M)
  , (2005, Geometry.WkbGeom Geometry.MultiLineString Geometry.M)
  , (2006, Geometry.WkbGeom Geometry.MultiPolygon Geometry.M)
  , (2007, Geometry.WkbGeom Geometry.GeometryCollection Geometry.M)
  , (3000, Geometry.WkbGeom Geometry.Geometry Geometry.ZM)
  , (3001, Geometry.WkbGeom Geometry.Point Geometry.ZM)
  , (3002, Geometry.WkbGeom Geometry.LineString Geometry.ZM)
  , (3003, Geometry.WkbGeom Geometry.Polygon Geometry.ZM)
  , (3004, Geometry.WkbGeom Geometry.MultiPoint Geometry.ZM)
  , (3005, Geometry.WkbGeom Geometry.MultiLineString Geometry.ZM)
  , (3006, Geometry.WkbGeom Geometry.MultiPolygon Geometry.ZM)
  , (3007, Geometry.WkbGeom Geometry.GeometryCollection Geometry.ZM)
  ]
