{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkt.BoxSpec where

import           Control.Lens    ((^?), (^?!))
import qualified Data.Geospatial as Geospatial
import qualified Data.Maybe      as Maybe
import           Test.Hspec      (Spec, describe, expectationFailure, it,
                                  shouldBe, shouldSatisfy)
import qualified Text.Trifecta   as Trifecta

import qualified Data.Wkt        as Wkt

spec :: Spec
spec =
  testBoxes

testBoxes :: Spec
testBoxes =
  describe "boxes" $ do
    it "Parse incomplete" $
      Wkt.parseString Wkt.box "box" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse not box" $
      Wkt.parseString Wkt.box "box (abc)" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse something" $ do
      let x = Wkt.parseString Wkt.box "box(145.1145 -37.7948, 145.1372 -37.7767)"
      case x of
        Trifecta.Success a -> a `shouldBe` exampleBox
        Trifecta.Failure f -> expectationFailure (show f)
    it "Parse spaces" $
      Wkt.parseString Wkt.box "box(145.1145  -37.7948,145.1372 -37.7767)" ^?! Trifecta._Success `shouldBe` exampleBox

exampleBox :: Geospatial.BoundingBoxWithoutCRS
exampleBox = Geospatial.BoundingBoxWithoutCRSXY (Geospatial.PointXY 145.1145 (-37.7948)) (Geospatial.PointXY 145.1372 (-37.7767))
