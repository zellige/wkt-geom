{-# LANGUAGE OverloadedStrings #-}

module Data.WkbPolygonSpec where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.Geospatial         as Geospatial
import           Data.Monoid             ((<>))
import qualified Data.Vector             as Vector
import           Test.Hspec              (Spec, describe, it, shouldBe)

import qualified Data.Wkb                as Wkb

import qualified Data.SpecHelper         as SpecHelper

spec :: Spec
spec = do
  testWkbPolygonParsing
  testWkbMultiPolygonParsing

testWkbPolygonParsing :: Spec
testWkbPolygonParsing =
  describe "Test wkb polygon" $ do
    it "Parse valid wkb polygon" $
      Wkb.parseByteString exampleWkbPolygon `shouldBe` (Right $ Geospatial.Polygon $ Geospatial.GeoPolygon (Vector.singleton SpecHelper.linearRing1))
    it "Not parse bad wkb polygon" $
      Wkb.parseByteString exampleBadWkbPolygon `shouldBe` Left "Could not parse wkb: First and last points of linear ring are different: first=GeoPointXY (PointXY {_xyX = 1.0, _xyY = 2.0}) last=GeoPointXY (PointXY {_xyX = 7.0, _xyY = 8.0})"

exampleWkbPolygon :: LazyByteString.ByteString
exampleWkbPolygon =
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
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0

exampleBadWkbPolygon :: LazyByteString.ByteString
exampleBadWkbPolygon =
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

testWkbMultiPolygonParsing :: Spec
testWkbMultiPolygonParsing =
  describe "Test wkb multi polygon" $
    it "Parse valid wkb multi polygon" $
      Wkb.parseByteString exampleWkbMultiPolygon `shouldBe` (Right $ Geospatial.MultiPolygon $ Geospatial.GeoMultiPolygon
        (Vector.fromList
          [ Vector.singleton SpecHelper.linearRing1
          , Vector.singleton SpecHelper.linearRing2
        ]))

exampleWkbMultiPolygon :: LazyByteString.ByteString
exampleWkbMultiPolygon =
  ByteStringBuilder.toLazyByteString $
    ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 6
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 3
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.int32BE 4
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0
    <> ByteStringBuilder.doubleBE 3.0
    <> ByteStringBuilder.doubleBE 4.0
    <> ByteStringBuilder.doubleBE 5.0
    <> ByteStringBuilder.doubleBE 6.0
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 3
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.int32BE 4
    <> ByteStringBuilder.doubleBE 1.5
    <> ByteStringBuilder.doubleBE 2.5
    <> ByteStringBuilder.doubleBE 3.5
    <> ByteStringBuilder.doubleBE 4.5
    <> ByteStringBuilder.doubleBE 5.5
    <> ByteStringBuilder.doubleBE 6.5
    <> ByteStringBuilder.doubleBE 1.5
    <> ByteStringBuilder.doubleBE 2.5
