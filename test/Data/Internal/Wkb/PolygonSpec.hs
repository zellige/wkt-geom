{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.PolygonSpec where

import qualified Data.ByteString.Builder     as ByteStringBuilder
import qualified Data.Geospatial             as Geospatial
import           Data.Monoid                 ((<>))
import qualified HaskellWorks.Hspec.Hedgehog as HedgehogHspec
import           Hedgehog
import           Test.Hspec                  (Spec, describe, it, shouldBe)

import qualified Data.Internal.Wkb.Geometry  as Geometry
import qualified Data.SpecHelper             as SpecHelper
import qualified Data.Wkb                    as Wkb

spec :: Spec
spec = do
  testWkbPolygonParsing
  testWkbMultiPolygonParsing


-- Test Wkb Polygon Parsing

testWkbPolygonParsing :: Spec
testWkbPolygonParsing =
  describe "Test wkb polygon" $ do
    testValidWkbPolyonParsing
    testInvalidWkbPolyonParsing

testValidWkbPolyonParsing :: Spec
testValidWkbPolyonParsing =
  mapM_ testValidWkbPolyonParsing' SpecHelper.coordPointGenerators

testValidWkbPolyonParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testValidWkbPolyonParsing' (coordType, genCoordPoint) =
  it ("round trips valid wkb polygon: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    polygon <- forAll $ SpecHelper.genPolygon genCoordPoint
    endianType <- forAll SpecHelper.genEndianType
    SpecHelper.roundTripWkb endianType polygon === Right polygon

testInvalidWkbPolyonParsing :: Spec
testInvalidWkbPolyonParsing =
  it "does not parse bad wkb polygon" $
    Wkb.parseByteString exampleBadWkbPolygon `shouldBe` Left "Could not parse wkb: First and last points of linear ring are different: first=GeoPointXY (PointXY {_xyX = 1.0, _xyY = 2.0}) last=GeoPointXY (PointXY {_xyX = 7.0, _xyY = 8.0})"
  where exampleBadWkbPolygon =
          ByteStringBuilder.toLazyByteString $
            ByteStringBuilder.word8 0
            <> ByteStringBuilder.int32BE 3
            <> ByteStringBuilder.int32BE 1
            <> ByteStringBuilder.int32BE 4
            <> ByteStringBuilder.doubleBE 1.0
            <> ByteStringBuilder.doubleBE 2.0
            <> ByteStringBuilder.doubleBE 3.0
            <> ByteStringBuilder.doubleBE 4.0
            <> ByteStringBuilder.doubleBE 5.0
            <> ByteStringBuilder.doubleBE 6.0
            <> ByteStringBuilder.doubleBE 7.0
            <> ByteStringBuilder.doubleBE 8.0


-- Test Wkb MultiPolygon Parsing

testWkbMultiPolygonParsing :: Spec
testWkbMultiPolygonParsing =
  describe "Test wkb multipolygon parsing" $
    mapM_ testWkbMultiPolygonParsing' SpecHelper.coordPointGenerators

testWkbMultiPolygonParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testWkbMultiPolygonParsing' (coordType, genCoordPoint) =
  it ("round trips wkb multipolygon: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    multiPolygon <- forAll $ SpecHelper.genMultiPolygon genCoordPoint
    endianType <- forAll SpecHelper.genEndianType
    SpecHelper.roundTripWkb endianType multiPolygon === Right multiPolygon
