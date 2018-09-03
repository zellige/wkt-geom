{-# LANGUAGE OverloadedStrings #-}

module Data.Wkb.PolygonSpec where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.Geospatial         as Geospatial
import qualified Data.LinearRing         as LinearRing
import           Data.Monoid             ((<>))
import           Test.Hspec              (Spec, describe, it, shouldBe)

import qualified Data.Wkb                as Wkb

spec :: Spec
spec = do
  testWkbPolygonParsing
  testWkbMultiPolygonParsing

testWkbPolygonParsing :: Spec
testWkbPolygonParsing =
  describe "Test wkb polygon" $ do
    it "Parse valid wkb polygon" $
      Wkb.parseByteString exampleWkbPolygon `shouldBe` (Right $ Geospatial.Polygon $ Geospatial.GeoPolygon [LinearRing.makeLinearRing [1.0,2.0] [3.0,4.0] [5.0,6.0] []])
    it "Not parse bad wkb polygon" $
      Wkb.parseByteString exampleBadWkbPolygon `shouldBe` Left "Could not parse wkb: First and last points of linear ring are different: first=[1.0,2.0] last=[7.0,8.0]"

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
      Wkb.parseByteString exampleWkbMultiPolygon `shouldBe` (Right $ Geospatial.MultiPolygon $ Geospatial.mergeGeoPolygons
        [ Geospatial.GeoPolygon [LinearRing.makeLinearRing [1.0,2.0] [3.0,4.0] [5.0,6.0] []]
        , Geospatial.GeoPolygon [LinearRing.makeLinearRing [1.5,2.5] [3.5,4.5] [5.5,6.5] []]])

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
