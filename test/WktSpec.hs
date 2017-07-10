{-# LANGUAGE OverloadedStrings #-}

module WktSpec where

import           Control.Lens
import           Data.Geography.GeoJSON
import           Data.Maybe
import           Test.Hspec             (Spec, describe, it, shouldBe,
                                         shouldSatisfy)
import           Text.Trifecta

import           GeometryCollection
import           Line
import           Point
import           Polygon
import           Wkt

spec :: Spec
spec = do
  testPoints
  testMultiPoints
  testLines
  testMultiLines
  testPolygons
  testMultiPolygons
  testGeometryCollection

testPoints :: Spec
testPoints =
  describe "simple points" $ do
    it "Parse incomplete" $
      Wkt.parseString Point.point "point" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse empty" $
      Wkt.parseString Point.point "point empty" ^?! _Success `shouldBe` Point.emptyPoint
    it "Parse not points" $
      Wkt.parseString Point.point "point (abc)" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse something" $
      Wkt.parseString Point.point "point (1.0 2.0)" ^?! _Success `shouldBe` PointGeometry [1.0, 2.0]
    it "Parse spaces" $
      Wkt.parseString Point.point "point( 1.0 2.0 )" ^?! _Success `shouldBe` PointGeometry [1.0, 2.0]

testMultiPoints :: Spec
testMultiPoints =
  describe "simple multipoints" $ do
    it "Parse incomplete" $
      Wkt.parseString Point.multiPoint "multipoint" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse empty" $
      Wkt.parseString Point.multiPoint "multipoint empty" ^?! _Success `shouldBe` Point.emptyMultiPoint
    it "Parse not points" $
      Wkt.parseString Point.multiPoint "multipoint (abc)" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse unbracketed" $
      Wkt.parseString Point.multiPoint "multipoint (1.0 2.0, 2.0 2.0)" ^?! _Success `shouldBe` MultiPointGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 2.0]]
    it "Parse unbraccketed spaces" $
      Wkt.parseString Point.multiPoint "multipoint( 1.0 2.0,2.0 2.0 )" ^?! _Success `shouldBe` MultiPointGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 2.0]]
    it "Parse bracketed" $
      Wkt.parseString Point.multiPoint "multipoint ((1.0 2.0), (2.0 2.0))" ^?! _Success `shouldBe` MultiPointGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 2.0]]
    it "Parse braccketed spaces" $
      Wkt.parseString Point.multiPoint "multipoint( ( 1.0 2.0) ,( 2.0 2.0) )" ^?! _Success `shouldBe` MultiPointGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 2.0]]

testLines :: Spec
testLines =
  describe "simple lines" $ do
    it "Parse incomplete" $
      Wkt.parseString Line.lineString "linestring" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse empty" $
      Wkt.parseString Line.lineString "linestring empty" ^?! _Success `shouldBe` Line.emptyLine
    it "Parse not points" $
      Wkt.parseString Line.lineString "linestring (abc)" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse something" $
      Wkt.parseString Line.lineString "linestring (1.0 2.0)" ^?! _Success `shouldBe` LineStringGeometry [PointGeometry [1.0, 2.0]]
    it "Parse spaces" $
      Wkt.parseString Line.lineString "linestring( 1.0 2.0 )" ^?! _Success `shouldBe` LineStringGeometry [PointGeometry [1.0, 2.0]]

testMultiLines :: Spec
testMultiLines =
  describe "simple multilines" $ do
    it "Parse incomplete" $
      Wkt.parseString Line.multiLineString "multilinestring" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse empty" $
      Wkt.parseString Line.multiLineString "multilinestring empty" ^?! _Success `shouldBe` Line.emptyMultiLine
    it "Parse not points" $
      Wkt.parseString Line.multiLineString "multilinestring (abc)" `shouldSatisfy` (isJust . flip (^?) _Failure)
    it "Parse something" $
      Wkt.parseString Line.multiLineString "multilinestring ((1.0 2.0, 2.0 3.0))" ^?! _Success `shouldBe` MultiLineStringGeometry [LineStringGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]]]
    it "Parse spaces" $
      Wkt.parseString Line.multiLineString "multilinestring( ( 1.0 2.0 ,2.0 3.0 ) )" ^?! _Success `shouldBe` MultiLineStringGeometry [LineStringGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]]]

testPolygons :: Spec
testPolygons =
  describe "simple polygons" $ do
    it "Parse empty" $
      Wkt.parseString Polygon.polygon "polygon empty" ^?! _Success `shouldBe` Polygon.emptyPolygon
    it "Parse something" $
      Wkt.parseString Polygon.polygon "polygon ((1.0 2.0, 2.0 3.0))" ^?! _Success `shouldBe` PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] []
    it "Parse spaces" $
      Wkt.parseString Polygon.polygon "polygon( (1.0 2.0,2.0 3.0) )" ^?! _Success `shouldBe` PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] []
    it "Parse something with hole" $
      Wkt.parseString Polygon.polygon "polygon ((1.0 2.0, 2.0 3.0), (1.1 1.9))" ^?! _Success `shouldBe` PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] [[PointGeometry [1.1, 1.9]]]
    it "Parse something with hole spaces" $
      Wkt.parseString Polygon.polygon "polygon( ( 1.0 2.0,2.0 3.0 ) ,( 1.1 1.9 ) )" ^?! _Success `shouldBe` PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] [[PointGeometry [1.1, 1.9]]]

testMultiPolygons :: Spec
testMultiPolygons =
  describe "simple multipolygons" $ do
    it "Parse empty" $
      Wkt.parseString Polygon.multiPolygon "multipolygon empty" ^?! _Success `shouldBe` Polygon.emptyMultiPolygon
    it "Parse something" $
      Wkt.parseString Polygon.multiPolygon "multipolygon (((1.0 2.0, 2.0 3.0)))" ^?! _Success `shouldBe` MultiPolygonGeometry [PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] []]
    it "Parse something with hole" $
      Wkt.parseString Polygon.multiPolygon "multipolygon ( ((1.0 2.0, 2.0 3.0), (1.1 1.9) ), ((10 10, 10 20), (60 60, 70 70 ) ))" ^?! _Success `shouldBe` MultiPolygonGeometry [PolygonGeometry [PointGeometry [1.0, 2.0], PointGeometry [2.0, 3.0]] [[PointGeometry [1.1, 1.9]]], PolygonGeometry [PointGeometry [10.0, 10.0], PointGeometry [10.0, 20.0]] [[PointGeometry [60, 60], PointGeometry [70, 70]]]]

testGeometryCollection :: Spec
testGeometryCollection =
  describe "simple geometry collections" $ do
    it "Parse empty" $
      Wkt.parseString GeometryCollection.geometryCollection "geometrycollection empty" ^?! _Success `shouldBe` GeometryCollection.emptyGeometryCollection
    it "Parse something" $
      Wkt.parseString GeometryCollection.geometryCollection "GeometryCollection(POINT (10 10),POINT (30 30),LINESTRING (15 15, 20 20))" ^?! _Success `shouldBe` [Point (PointGeometry [10.0, 10.0]), Point (PointGeometry [30.0, 30.0]), LineString(LineStringGeometry [PointGeometry [15.0, 15.0], PointGeometry [20.0, 20.0]]) ]
    it "Parse something with spaces" $
      Wkt.parseString GeometryCollection.geometryCollection "GeometryCollection( POINT(10 10), POINT(30 30), LINESTRING( 15 15, 20 20 ) )" ^?! _Success `shouldBe` [Point (PointGeometry [10.0, 10.0]), Point (PointGeometry [30.0, 30.0]), LineString(LineStringGeometry [PointGeometry [15.0, 15.0], PointGeometry [20.0, 20.0]]) ]
