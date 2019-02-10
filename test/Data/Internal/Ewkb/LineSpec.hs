{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Ewkb.LineSpec where

import           Test.Hspec      (Spec, describe)

import qualified Data.SpecHelper as SpecHelper

spec :: Spec
spec = do
  testEwkbLineParsing
  testEwkbMultiLineParsing


-- Test Wkb Line Parsing

testEwkbLineParsing :: Spec
testEwkbLineParsing =
  describe "Test ewkb line parsing" $
    SpecHelper.testRoundTripEwkbGeometryParsing "line" SpecHelper.genLine


-- Test Wkb MultiLine Parsing

testEwkbMultiLineParsing :: Spec
testEwkbMultiLineParsing =
  describe "Test ewkb multiline parsing" $
    SpecHelper.testRoundTripEwkbGeometryParsing "multiLine" SpecHelper.genMultiLine
