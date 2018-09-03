{-# LANGUAGE OverloadedStrings #-}

module Data.Wkb.GeometryCollectionSpec where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.Geospatial         as Geospatial
import qualified Data.LineString         as LineString
import           Data.Monoid             ((<>))
import           Test.Hspec              (Spec, describe, it, shouldBe)

import qualified Data.Wkb                as Wkb

spec :: Spec
spec =
  testWkbGeometryCollectionParsing

testWkbGeometryCollectionParsing :: Spec
testWkbGeometryCollectionParsing =
  describe "Test wkb geometry collection" $
    it "Parse valid wkb geometry collection" $
      Wkb.parseByteString exampleWkbGeometryCollection `shouldBe` (Right $ Geospatial.Collection $
        [ Geospatial.Point $ Geospatial.GeoPoint [10, 10]
        , Geospatial.Point $ Geospatial.GeoPoint [30, 30]
        , Geospatial.Line $ Geospatial.GeoLine $ LineString.makeLineString [15, 15] [20, 20] [[25, 25]]
        ])

exampleWkbGeometryCollection :: LazyByteString.ByteString
exampleWkbGeometryCollection =
  ByteStringBuilder.toLazyByteString $
    ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 7
    <> ByteStringBuilder.int32BE 3
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.doubleBE 10
    <> ByteStringBuilder.doubleBE 10
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.doubleBE 30
    <> ByteStringBuilder.doubleBE 30
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.int32BE 3
    <> ByteStringBuilder.doubleBE 15
    <> ByteStringBuilder.doubleBE 15
    <> ByteStringBuilder.doubleBE 20
    <> ByteStringBuilder.doubleBE 20
    <> ByteStringBuilder.doubleBE 25
    <> ByteStringBuilder.doubleBE 25
