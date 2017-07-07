{-# LANGUAGE OverloadedStrings #-}

module WktSpec where

import           Control.Lens
import           Data.Geography.GeoJSON
import           Data.Maybe
import           Test.Hspec             (Spec, describe, it, shouldBe,
                                         shouldSatisfy)
import           Text.Trifecta

import           Lines
import           Points
import           Polygons
import           Wkt

spec :: Spec
spec = do
  testPoints
  testLines
  testPolygons

testPoints :: Spec
testPoints =
  describe "simple points" $ do
    it "Parse incomplete" $
      (\str -> parseString pointTaggedText (Wkt.delta str) str) "point" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse empty" $
      (\str -> parseString pointTaggedText (Wkt.delta str) str) "point empty" ^?! _Success `shouldBe` Points.emptyPoint
    it "Parse not points" $
      (\str -> parseString pointTaggedText (Wkt.delta str) str) "point (abc)" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse something" $
      (\str -> parseString pointTaggedText (Wkt.delta str) str) "point (1.0 2.0)" ^?! _Success `shouldBe` PointGeometry [1.0, 2.0]

testLines :: Spec
testLines =
  describe "simple lines" $ do
    it "Parse incomplete" $
      (\str -> parseString lineStringText (Wkt.delta str) str) "linestring" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse empty" $
      (\str -> parseString lineStringText (Wkt.delta str) str) "linestring empty" ^?! _Success `shouldBe` Lines.emptyLine
    it "Parse not points" $
      (\str -> parseString lineStringText (Wkt.delta str) str) "linestring (abc)" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse something" $
      (\str -> parseString lineStringText (Wkt.delta str) str) "linestring (1.0 2.0)" ^?! _Success `shouldBe` LineStringGeometry [PointGeometry [1.0, 2.0]]

testPolygons :: Spec
testPolygons =
  describe "simple polygons" $ do
    it "Parse empty" $
      (\str -> parseString polygonText (Wkt.delta str) str) "polygon empty" ^?! _Success `shouldBe` Polygons.emptyPolygon
    it "Parse something" $
      (\str -> parseString polygonText (Wkt.delta str) str) "polygon ((1.0 2.0, 2.0 3.0))" ^?! _Success `shouldBe` PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] []
    it "Parse something with hole" $
      (\str -> parseString polygonText (Wkt.delta str) str) "polygon ((1.0 2.0, 2.0 3.0), (1.1 1.9))" ^?! _Success `shouldBe` PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] [[PointGeometry [1.1, 1.9]]]
