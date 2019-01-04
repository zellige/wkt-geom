{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.LineSpec where

import qualified Data.Geospatial             as Geospatial
import qualified HaskellWorks.Hspec.Hedgehog as HedgehogHspec
import           Hedgehog
import           Test.Hspec                  (Spec, describe, it)

import qualified Data.Internal.Wkb.Geometry  as Geometry
import qualified Data.SpecHelper             as SpecHelper

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
    line <- forAll $ SpecHelper.genLine genCoordPoint
    endianType <- forAll SpecHelper.genEndianType
    SpecHelper.roundTripWkb endianType line === Right line


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
    SpecHelper.roundTripWkb endianType multiLine === Right multiLine
