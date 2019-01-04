{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.GeometrySpec where

import qualified Data.Binary.Get            as BinaryGet
import qualified Data.ByteString.Builder    as ByteStringBuilder
import           Test.Hspec                 (Spec, describe, it, shouldBe)

import qualified Data.Internal.Wkb.Endian   as Endian
import qualified Data.Internal.Wkb.Geometry as Geometry

spec :: Spec
spec =
  testValidGetGeometryTypeWithCoords

testValidGetGeometryTypeWithCoords :: Spec
testValidGetGeometryTypeWithCoords =
  describe "get geometry type with coords type for valid data" $
    mapM_ testGetGeometryTypeWithCoords geometryTypes

testGetGeometryTypeWithCoords :: Geometry.WkbGeometryType -> Spec
testGetGeometryTypeWithCoords geometryType =
  it ("Parse " ++ show geometryType) $
    mapM_ test [Endian.BigEndian, Endian.LittleEndian]
    where test endianType =
            roundTrip endianType geometryType `shouldBe` geometryType

roundTrip :: Endian.EndianType -> Geometry.WkbGeometryType -> Geometry.WkbGeometryType
roundTrip endianType geometryType =
  BinaryGet.runGet (Geometry.getWkbGeom endianType) encodedGeometryType
  where encodedGeometryType = ByteStringBuilder.toLazyByteString $ Geometry.builderWkbGeom endianType geometryType

geometryTypes :: [Geometry.WkbGeometryType]
geometryTypes =
  [ Geometry.WkbGeom Geometry.Geometry Geometry.TwoD
  , Geometry.WkbGeom Geometry.Point Geometry.TwoD
  , Geometry.WkbGeom Geometry.LineString Geometry.TwoD
  , Geometry.WkbGeom Geometry.Polygon Geometry.TwoD
  , Geometry.WkbGeom Geometry.MultiPoint Geometry.TwoD
  , Geometry.WkbGeom Geometry.MultiLineString Geometry.TwoD
  , Geometry.WkbGeom Geometry.MultiPolygon Geometry.TwoD
  , Geometry.WkbGeom Geometry.GeometryCollection Geometry.TwoD
  , Geometry.WkbGeom Geometry.Geometry Geometry.Z
  , Geometry.WkbGeom Geometry.Point Geometry.Z
  , Geometry.WkbGeom Geometry.LineString Geometry.Z
  , Geometry.WkbGeom Geometry.Polygon Geometry.Z
  , Geometry.WkbGeom Geometry.MultiPoint Geometry.Z
  , Geometry.WkbGeom Geometry.MultiLineString Geometry.Z
  , Geometry.WkbGeom Geometry.MultiPolygon Geometry.Z
  , Geometry.WkbGeom Geometry.GeometryCollection Geometry.Z
  , Geometry.WkbGeom Geometry.Geometry Geometry.M
  , Geometry.WkbGeom Geometry.Point Geometry.M
  , Geometry.WkbGeom Geometry.LineString Geometry.M
  , Geometry.WkbGeom Geometry.Polygon Geometry.M
  , Geometry.WkbGeom Geometry.MultiPoint Geometry.M
  , Geometry.WkbGeom Geometry.MultiLineString Geometry.M
  , Geometry.WkbGeom Geometry.MultiPolygon Geometry.M
  , Geometry.WkbGeom Geometry.GeometryCollection Geometry.M
  , Geometry.WkbGeom Geometry.Geometry Geometry.ZM
  , Geometry.WkbGeom Geometry.Point Geometry.ZM
  , Geometry.WkbGeom Geometry.LineString Geometry.ZM
  , Geometry.WkbGeom Geometry.Polygon Geometry.ZM
  , Geometry.WkbGeom Geometry.MultiPoint Geometry.ZM
  , Geometry.WkbGeom Geometry.MultiLineString Geometry.ZM
  , Geometry.WkbGeom Geometry.MultiPolygon Geometry.ZM
  , Geometry.WkbGeom Geometry.GeometryCollection Geometry.ZM
  ]
