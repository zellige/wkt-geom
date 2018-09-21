{-# LANGUAGE OverloadedStrings #-}

module Data.WkbLineSpec where

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
  testWkbLineParsing
  testWkbMultiLineParsing

testWkbLineParsing :: Spec
testWkbLineParsing =
  describe "Test wkb line" $
    it "Parse valid wkb line" $
      Wkb.parseByteString exampleWkbLine `shouldBe` (Right $ Geospatial.Line $ Geospatial.GeoLine SpecHelper.lineString1)

exampleWkbLine :: LazyByteString.ByteString
exampleWkbLine =
  ByteStringBuilder.toLazyByteString $
    ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0
    <> ByteStringBuilder.doubleBE 3.0
    <> ByteStringBuilder.doubleBE 4.0

testWkbMultiLineParsing :: Spec
testWkbMultiLineParsing =
  describe "Test wkb multi line" $
    it "Parse valid wkb multi line" $
      Wkb.parseByteString exampleWkbMultiLine `shouldBe` (Right $ Geospatial.MultiLine $ Geospatial.GeoMultiLine (Vector.fromList [SpecHelper.lineString1, SpecHelper.lineString2]))

exampleWkbMultiLine :: LazyByteString.ByteString
exampleWkbMultiLine =
  ByteStringBuilder.toLazyByteString $
    ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 5
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0
    <> ByteStringBuilder.doubleBE 3.0
    <> ByteStringBuilder.doubleBE 4.0
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.doubleBE 1.5
    <> ByteStringBuilder.doubleBE 2.5
    <> ByteStringBuilder.doubleBE 3.5
    <> ByteStringBuilder.doubleBE 4.5
