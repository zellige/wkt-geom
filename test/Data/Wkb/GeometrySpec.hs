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

testGetGeometryTypeWithCoords :: (Word.Word32, Geometry.WkbGeometryTypeWithCoords) -> Spec
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

expectations :: [(Word.Word32, Geometry.WkbGeometryTypeWithCoords)]
expectations =
  [ (0000, Geometry.WkbGeom Geometry.WkbGeometry Geometry.TwoD)
  , (0001, Geometry.WkbGeom Geometry.WkbPoint Geometry.TwoD)
  , (0002, Geometry.WkbGeom Geometry.WkbLineString Geometry.TwoD)
  , (0003, Geometry.WkbGeom Geometry.WkbPolygon Geometry.TwoD)
  , (0004, Geometry.WkbGeom Geometry.WkbMultiPoint Geometry.TwoD)
  , (0005, Geometry.WkbGeom Geometry.WkbMultiLineString Geometry.TwoD)
  , (0006, Geometry.WkbGeom Geometry.WkbMultiPolygon Geometry.TwoD)
  , (0007, Geometry.WkbGeom Geometry.WkbGeometryCollection Geometry.TwoD)
  , (1000, Geometry.WkbGeom Geometry.WkbGeometry Geometry.Z)
  , (1001, Geometry.WkbGeom Geometry.WkbPoint Geometry.Z)
  , (1002, Geometry.WkbGeom Geometry.WkbLineString Geometry.Z)
  , (1003, Geometry.WkbGeom Geometry.WkbPolygon Geometry.Z)
  , (1004, Geometry.WkbGeom Geometry.WkbMultiPoint Geometry.Z)
  , (1005, Geometry.WkbGeom Geometry.WkbMultiLineString Geometry.Z)
  , (1006, Geometry.WkbGeom Geometry.WkbMultiPolygon Geometry.Z)
  , (1007, Geometry.WkbGeom Geometry.WkbGeometryCollection Geometry.Z)
  , (2000, Geometry.WkbGeom Geometry.WkbGeometry Geometry.M)
  , (2001, Geometry.WkbGeom Geometry.WkbPoint Geometry.M)
  , (2002, Geometry.WkbGeom Geometry.WkbLineString Geometry.M)
  , (2003, Geometry.WkbGeom Geometry.WkbPolygon Geometry.M)
  , (2004, Geometry.WkbGeom Geometry.WkbMultiPoint Geometry.M)
  , (2005, Geometry.WkbGeom Geometry.WkbMultiLineString Geometry.M)
  , (2006, Geometry.WkbGeom Geometry.WkbMultiPolygon Geometry.M)
  , (2007, Geometry.WkbGeom Geometry.WkbGeometryCollection Geometry.M)
  , (3000, Geometry.WkbGeom Geometry.WkbGeometry Geometry.ZM)
  , (3001, Geometry.WkbGeom Geometry.WkbPoint Geometry.ZM)
  , (3002, Geometry.WkbGeom Geometry.WkbLineString Geometry.ZM)
  , (3003, Geometry.WkbGeom Geometry.WkbPolygon Geometry.ZM)
  , (3004, Geometry.WkbGeom Geometry.WkbMultiPoint Geometry.ZM)
  , (3005, Geometry.WkbGeom Geometry.WkbMultiLineString Geometry.ZM)
  , (3006, Geometry.WkbGeom Geometry.WkbMultiPolygon Geometry.ZM)
  , (3007, Geometry.WkbGeom Geometry.WkbGeometryCollection Geometry.ZM)
  ]
