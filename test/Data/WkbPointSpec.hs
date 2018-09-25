{-# LANGUAGE OverloadedStrings #-}

module Data.WkbPointSpec where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.Geospatial         as Geospatial
import           Data.Monoid             ((<>))
import qualified Data.Sequence           as Sequence
import           Test.Hspec              (Spec, describe, it, shouldBe)

import qualified Data.Wkb                as Wkb

import qualified Data.SpecHelper         as SpecHelper

spec :: Spec
spec = do
  testWkbPointParsing
  testWkbMultiPointParsing

testWkbPointParsing :: Spec
testWkbPointParsing =
  describe "Test wkb point" $
    it "Parse valid wkb point" $
      Wkb.parseByteString exampleWkbPoint `shouldBe` (Right . Geospatial.Point $ Geospatial.GeoPoint SpecHelper.point1)

exampleWkbPoint :: LazyByteString.ByteString
exampleWkbPoint =
  ByteStringBuilder.toLazyByteString $
    ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0

testWkbMultiPointParsing :: Spec
testWkbMultiPointParsing =
  describe "Test wkb multi point" $
    it "Parse valid wkb multi point" $
      Wkb.parseByteString exampleWkbMultiPoint `shouldBe` (Right $ Geospatial.MultiPoint (Geospatial.GeoMultiPoint (Sequence.fromList [SpecHelper.point1, SpecHelper.point2])))

exampleWkbMultiPoint :: LazyByteString.ByteString
exampleWkbMultiPoint =
  ByteStringBuilder.toLazyByteString $
    ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 4
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.doubleBE 3.0
    <> ByteStringBuilder.doubleBE 4.0

