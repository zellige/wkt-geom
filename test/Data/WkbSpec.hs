{-# LANGUAGE OverloadedStrings #-}

module Data.WkbSpec where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.Geospatial         as Geospatial
import           Data.Monoid             ((<>))
import           Test.Hspec              (Spec, describe, it, shouldBe)

import qualified Data.Wkb                as Wkb

spec :: Spec
spec =
  testGetEndian

testGetEndian :: Spec
testGetEndian =
  describe "get geospatial geom" $
    it "Parse point wkb" $
      Wkb.parseByteString testWkb `shouldBe` (Right $ Geospatial.Point $ Geospatial.GeoPoint [1.0, 2.0])

testWkb :: LazyByteString.ByteString
testWkb =
  ByteStringBuilder.toLazyByteString $
    ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0
