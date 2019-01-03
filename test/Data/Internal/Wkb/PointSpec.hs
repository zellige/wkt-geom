{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Internal.Wkb.PointSpec where

import qualified Data.Binary.Get              as BinaryGet
import qualified Data.ByteString.Builder      as ByteStringBuilder
import qualified Data.ByteString.Lazy         as LazyByteString
import qualified Data.Geospatial              as Geospatial
import           Data.Monoid                  ((<>))
import qualified Data.Sequence                as Sequence
import qualified Data.SpecHelper              as SpecHelper
import qualified HaskellWorks.Hspec.Hedgehog  as HedgehogHspec
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range
import           Test.Hspec                   (Spec, describe, it, shouldBe)

import qualified Data.Internal.Wkb.EndianSpec as EndianSpec
import qualified Data.Internal.Wkb.Geometry   as Geometry
import qualified Data.Internal.Wkb.Point      as Point
import qualified Data.Wkb                     as Wkb

spec :: Spec
spec = do
  testCoordPointParsing
  testWkbPointParsing
  testWkbMultiPointParsing

testCoordPointParsing :: Spec
testCoordPointParsing =
  describe "Test coord point" $
    it "Round trips coord point" $ HedgehogHspec.require $ property $ do
      coordPoint <- forAll genGeoPointXY
      endianType <- forAll EndianSpec.genEndianType
      roundTrip endianType coordPoint === coordPoint
  where
    roundTrip endianType coordPoint  =
      BinaryGet.runGet (Point.getCoordPoint endianType Geometry.TwoD) (encodedCoordPoint endianType coordPoint)
    encodedCoordPoint endianType coordPoint =
      ByteStringBuilder.toLazyByteString $ Point.builderCoordPoint endianType coordPoint

genGeoPointXY :: Gen Geospatial.GeoPositionWithoutCRS
genGeoPointXY = do
  x <- genDouble
  y <- genDouble
  return $ Geospatial.GeoPointXY (Geospatial.PointXY x y)
  where genDouble = Gen.double $ Range.linearFrac (-10000.0) 10000.0

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

