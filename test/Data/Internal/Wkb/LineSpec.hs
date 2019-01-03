{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.LineSpec where

import qualified Data.ByteString.Builder     as ByteStringBuilder
import qualified Data.ByteString.Lazy        as LazyByteString
import qualified Data.Geospatial             as Geospatial
import           Data.Monoid                 ((<>))
import qualified Data.Sequence               as Sequence
import qualified HaskellWorks.Hspec.Hedgehog as HedgehogHspec
import           Hedgehog
import           Test.Hspec                  (Spec, describe, it, shouldBe)

import qualified Data.Internal.Wkb.Geometry  as Geometry
import qualified Data.Internal.Wkb.Line      as Line
import qualified Data.SpecHelper             as SpecHelper
import qualified Data.Wkb                    as Wkb

spec :: Spec
spec = do
  testWkbLineParsing
  testWkbMultiLineParsing


-- Test Wkb Line Parsing

testWkbLineParsing :: Spec
testWkbLineParsing =
  describe "Test wkb line parsing" $
    mapM_ testWkbLineParsing' SpecHelper.coordPointGenerators

testWkbLineParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testWkbLineParsing' (coordType, genCoordPoint) =
  it ("round trips wkb line: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    line <- forAll $ Geospatial.GeoLine <$> SpecHelper.genLineString genCoordPoint
    endianType <- forAll SpecHelper.genEndianType
    roundTrip endianType line === (Right $ Geospatial.Line line)
  where roundTrip endianType =
          Wkb.parseByteString . ByteStringBuilder.toLazyByteString . Line.builderLine endianType


-- Test Wkb MultiLine Parsing

testWkbMultiLineParsing :: Spec
testWkbMultiLineParsing =
  describe "Test wkb multiline parsing" $
    it "Parse valid wkb multi line" $
      Wkb.parseByteString exampleWkbMultiLine `shouldBe` (Right $ Geospatial.MultiLine $ Geospatial.GeoMultiLine (Sequence.fromList [SpecHelper.lineString1, SpecHelper.lineString2]))

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
