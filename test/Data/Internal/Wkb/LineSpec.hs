{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.LineSpec where

import           Test.Hspec      (Spec, describe)

import qualified Data.SpecHelper as SpecHelper

spec :: Spec
spec = do
  testWkbLineParsing
  testWkbMultiLineParsing


-- Test Wkb Line Parsing

testWkbLineParsing :: Spec
testWkbLineParsing =
  describe "Test wkb line parsing" $
    SpecHelper.testRoundTripWkbGeometryParsing "line" SpecHelper.genLine


-- Test Wkb MultiLine Parsing

testWkbMultiLineParsing :: Spec
testWkbMultiLineParsing =
  describe "Test wkb multiline parsing" $
    SpecHelper.testRoundTripWkbGeometryParsing "multiLine" SpecHelper.genMultiLine
