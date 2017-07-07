{-# LANGUAGE OverloadedStrings #-}

module WktSpec where

import           Control.Lens
import           Data.Geography.GeoJSON
import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Text.Trifecta

import           Wkt

spec :: Spec
spec = do
  testPoints
  testLines
  testPolygons

testPoints :: Spec
testPoints =
  describe "simple points" $ do
    it "Parse empty" $
      (\str -> parseString pointText (Wkt.delta str) str) "point empty" ^?! _Success `shouldBe` Wkt.emptyPoint
    it "Parse something" $
      (\str -> parseString pointText (Wkt.delta str) str) "point (1.0 2.0)" ^?! _Success `shouldBe` PointGeometry [1.0, 2.0]

testLines :: Spec
testLines =
  describe "simple lines" $ do
    it "Parse empty" $
      (\str -> parseString lineStringText (Wkt.delta str) str) "linestring empty" ^?! _Success `shouldBe` Wkt.emptyLine
    it "Parse something" $
      (\str -> parseString lineStringText (Wkt.delta str) str) "linestring (1.0 2.0)" ^?! _Success `shouldBe` LineStringGeometry [PointGeometry [1.0, 2.0]]

testPolygons :: Spec
testPolygons =
  describe "simple polygons" $ do
    it "Parse empty" $
      (\str -> parseString polygonText (Wkt.delta str) str) "polygon empty" ^?! _Success `shouldBe` Wkt.emptyPolygon
    it "Parse something" $
      (\str -> parseString polygonText (Wkt.delta str) str) "polygon ((1.0 2.0, 2.0 3.0))" ^?! _Success `shouldBe` PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] []
