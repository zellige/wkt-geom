{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Ewkb.PointSpec where

import           Test.Hspec      (Spec, describe)

import qualified Data.SpecHelper as SpecHelper

spec :: Spec
spec = do
  testEwkbPointParsing
  testEwkbMultiPointParsing


-- Test Wkb Point Parsing

testEwkbPointParsing :: Spec
testEwkbPointParsing =
  describe "Test ewkb point parsing" $
    SpecHelper.testRoundTripEwkbGeometryParsing "point" SpecHelper.genPoint


-- Test Wkb MultiPoint Parsing

testEwkbMultiPointParsing :: Spec
testEwkbMultiPointParsing =
  describe "Test ewkb multipoint parsing" $
    SpecHelper.testRoundTripEwkbGeometryParsing "multipoint" SpecHelper.genMultiPoint
