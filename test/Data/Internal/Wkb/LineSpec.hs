{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.LineSpec where

import qualified Data.ByteString.Builder     as ByteStringBuilder
import qualified Data.Geospatial             as Geospatial
import qualified HaskellWorks.Hspec.Hedgehog as HedgehogHspec
import           Hedgehog
import           Test.Hspec                  (Spec, describe, it)

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
    mapM_ testWkbMultiLineParsing' SpecHelper.coordPointGenerators

testWkbMultiLineParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testWkbMultiLineParsing' (coordType, genCoordPoint) =
  it ("round trips wkb multiline: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    multiLine <- forAll $ SpecHelper.genMultiLine genCoordPoint
    endianType <- forAll SpecHelper.genEndianType
    roundTrip endianType multiLine === (Right $ Geospatial.MultiLine multiLine)
  where roundTrip endianType =
          Wkb.parseByteString . ByteStringBuilder.toLazyByteString . Line.builderMultiLine endianType
