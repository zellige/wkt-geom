{-# LANGUAGE OverloadedStrings #-}

module Data.WkbSpec where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.Geospatial         as Geospatial
import           Data.Monoid             ((<>))
import           Test.Hspec              (Spec, describe, it, shouldBe)

import qualified Data.Wkb                as Wkb

spec :: Spec
spec = do
  testWkbPointParsing
  testWkbMultiPointParsing

testWkbPointParsing :: Spec
testWkbPointParsing =
  describe "Test wkb geom" $
    it "Parse point wkb" $
      Wkb.parseByteString exampleWkbPoint `shouldBe` (Right $ Geospatial.Point $ Geospatial.GeoPoint [1.0, 2.0])

exampleWkbPoint :: LazyByteString.ByteString
exampleWkbPoint =
  ByteStringBuilder.toLazyByteString $
    ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0

testWkbMultiPointParsing :: Spec
testWkbMultiPointParsing =
  describe "Test wkb geom" $
    it "Parse point wkb" $
      Wkb.parseByteString exampleWkbMultiPoint `shouldBe` (Right $ Geospatial.MultiPoint $ Geospatial.mergeGeoPoints [Geospatial.GeoPoint [1.0, 2.0], Geospatial.GeoPoint [3.0, 4.0]])

exampleWkbMultiPoint :: LazyByteString.ByteString
exampleWkbMultiPoint =
  ByteStringBuilder.toLazyByteString $
    ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 4
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0
    <> ByteStringBuilder.doubleBE 3.0
    <> ByteStringBuilder.doubleBE 4.0
