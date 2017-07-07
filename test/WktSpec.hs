{-# LANGUAGE OverloadedStrings #-}

module WktSpec where

import           Control.Lens
import           Data.Geography.GeoJSON
import           Data.Maybe
import           Test.Hspec             (Spec, describe, it, shouldBe,
                                         shouldSatisfy)
import           Text.Trifecta

import           Line
import           Point
import           Polygon
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
      (\str -> parseString point (Wkt.delta str) str) "point" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse empty" $
      (\str -> parseString point (Wkt.delta str) str) "point empty" ^?! _Success `shouldBe` Point.emptyPoint
    it "Parse not points" $
      (\str -> parseString point (Wkt.delta str) str) "point (abc)" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse something" $
      (\str -> parseString point (Wkt.delta str) str) "point (1.0 2.0)" ^?! _Success `shouldBe` PointGeometry [1.0, 2.0]

testLines :: Spec
testLines =
  describe "simple lines" $ do
    it "Parse incomplete" $
      (\str -> parseString Line.lineString (Wkt.delta str) str) "linestring" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse empty" $
      (\str -> parseString Line.lineString (Wkt.delta str) str) "linestring empty" ^?! _Success `shouldBe` Line.emptyLine
    it "Parse not points" $
      (\str -> parseString Line.lineString (Wkt.delta str) str) "linestring (abc)" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse something" $
      (\str -> parseString Line.lineString (Wkt.delta str) str) "linestring (1.0 2.0)" ^?! _Success `shouldBe` LineStringGeometry [PointGeometry [1.0, 2.0]]

testPolygons :: Spec
testPolygons =
  describe "simple polygons" $ do
    it "Parse empty" $
      (\str -> parseString polygon (Wkt.delta str) str) "polygon empty" ^?! _Success `shouldBe` Polygon.emptyPolygon
    it "Parse something" $
      (\str -> parseString polygon (Wkt.delta str) str) "polygon ((1.0 2.0, 2.0 3.0))" ^?! _Success `shouldBe` PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] []
    it "Parse something with hole" $
      (\str -> parseString polygon (Wkt.delta str) str) "polygon ((1.0 2.0, 2.0 3.0), (1.1 1.9))" ^?! _Success `shouldBe` PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] [[PointGeometry [1.1, 1.9]]]
