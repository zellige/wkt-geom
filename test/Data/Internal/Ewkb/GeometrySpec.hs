{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Ewkb.GeometrySpec where

import qualified Data.Binary.Get             as BinaryGet
import qualified Data.ByteString.Builder     as ByteStringBuilder
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
    mapM_ testGetEwkbGeometryType testValues

testGetEwkbGeometryType :: Ewkb.EwkbGeometryType -> Spec
testGetEwkbGeometryType geometryType =
  it ("Parse " ++ show geometryType) $
    mapM_ test [Endian.BigEndian, Endian.LittleEndian]
    where test endianType = roundTrip endianType geometryType `shouldBe` geometryType

roundTrip :: Endian.EndianType -> Ewkb.EwkbGeometryType -> Ewkb.EwkbGeometryType
roundTrip endianType geometryType =
  BinaryGet.runGet (Ewkb.getEwkbGeometryType endianType) encodedGeometryType
  where encodedGeometryType = ByteStringBuilder.toLazyByteString $ Ewkb.builderEwkbGeometryType endianType geometryType

testValues :: [Ewkb.EwkbGeometryType]
testValues =
  [ Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.TwoD) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.TwoD) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.TwoD) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.TwoD) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.TwoD) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.TwoD) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.TwoD) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.TwoD) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.Z) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.Z) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.Z) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.Z) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.Z) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.Z) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.Z) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.Z) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.M) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.M) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.M) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.M) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.M) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.M) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.M) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.M) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.ZM) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.ZM) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.ZM) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.ZM) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.ZM) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.ZM) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.ZM) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.ZM) Ewkb.NoSrid
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.TwoD) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.TwoD) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.TwoD) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.TwoD) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.TwoD) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.TwoD) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.TwoD) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.TwoD) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.Z) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.Z) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.Z) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.Z) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.Z) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.Z) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.Z) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.Z) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.M) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.M) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.M) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.M) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.M) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.M) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.M) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.M) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Geometry Wkb.ZM) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Point Wkb.ZM) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.LineString Wkb.ZM) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.Polygon Wkb.ZM) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPoint Wkb.ZM) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiLineString Wkb.ZM) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.MultiPolygon Wkb.ZM) (Ewkb.Srid srid)
  , Ewkb.EwkbGeom (Wkb.WkbGeom Wkb.GeometryCollection Wkb.ZM) (Ewkb.Srid srid)
  ]

srid :: Word.Word32
srid = 4326
