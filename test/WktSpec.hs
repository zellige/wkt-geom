{-# LANGUAGE OverloadedStrings #-}

module WktSpec where

import           Control.Lens           ((^?), (^?!))
import qualified Data.Geography.GeoJSON as GeoJSON
import qualified Data.Maybe             as Maybe
import           Test.Hspec             (Spec, describe, it, shouldBe,
                                         shouldSatisfy)
import qualified Text.Trifecta          as Trifecta

import qualified GeometryCollection
import qualified Line
import qualified Point
import qualified Polygon
import qualified Wkt

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
      Wkt.parseString Point.point "point" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse empty" $
      Wkt.parseString Point.point "point empty" ^?! Trifecta._Success `shouldBe` Point.emptyPoint
    it "Parse not points" $
      Wkt.parseString Point.point "point (abc)" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse something" $
      Wkt.parseString Point.point "point (1.0 2.0)" ^?! Trifecta._Success `shouldBe` GeoJSON.PointGeometry [1.0, 2.0]
    it "Parse spaces" $
      Wkt.parseString Point.point "point( 1.0 2.0 )" ^?! Trifecta._Success `shouldBe` GeoJSON.PointGeometry [1.0, 2.0]

testMultiPoints :: Spec
testMultiPoints =
  describe "simple multipoints" $ do
    it "Parse incomplete" $
      Wkt.parseString Point.multiPoint "multipoint" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse empty" $
      Wkt.parseString Point.multiPoint "multipoint empty" ^?! Trifecta._Success `shouldBe` Point.emptyMultiPoint
    it "Parse not points" $
      Wkt.parseString Point.multiPoint "multipoint (abc)" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse unbracketed" $
      Wkt.parseString Point.multiPoint "multipoint (1.0 2.0, 2.0 2.0)" ^?! Trifecta._Success `shouldBe` GeoJSON.MultiPointGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 2.0]]
    it "Parse unbraccketed spaces" $
      Wkt.parseString Point.multiPoint "multipoint( 1.0 2.0,2.0 2.0 )" ^?! Trifecta._Success `shouldBe` GeoJSON.MultiPointGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 2.0]]
    it "Parse bracketed" $
      Wkt.parseString Point.multiPoint "multipoint ((1.0 2.0), (2.0 2.0))" ^?! Trifecta._Success `shouldBe` GeoJSON.MultiPointGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 2.0]]
    it "Parse braccketed spaces" $
      Wkt.parseString Point.multiPoint "multipoint( ( 1.0 2.0) ,( 2.0 2.0) )" ^?! Trifecta._Success `shouldBe` GeoJSON.MultiPointGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 2.0]]

testLines :: Spec
testLines =
  describe "simple lines" $ do
    it "Parse incomplete" $
      Wkt.parseString Line.lineString "linestring" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse empty" $
      Wkt.parseString Line.lineString "linestring empty" ^?! Trifecta._Success `shouldBe` Line.emptyLine
    it "Parse not points" $
      Wkt.parseString Line.lineString "linestring (abc)" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse something" $
      Wkt.parseString Line.lineString "linestring (1.0 2.0)" ^?! Trifecta._Success `shouldBe` GeoJSON.LineStringGeometry [GeoJSON.PointGeometry [1.0, 2.0]]
    it "Parse spaces" $
      Wkt.parseString Line.lineString "linestring( 1.0 2.0 )" ^?! Trifecta._Success `shouldBe` GeoJSON.LineStringGeometry [GeoJSON.PointGeometry [1.0, 2.0]]

testMultiLines :: Spec
testMultiLines =
  describe "simple multilines" $ do
    it "Parse incomplete" $
      Wkt.parseString Line.multiLineString "multilinestring" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse empty" $
      Wkt.parseString Line.multiLineString "multilinestring empty" ^?! Trifecta._Success `shouldBe` Line.emptyMultiLine
    it "Parse not points" $
      Wkt.parseString Line.multiLineString "multilinestring (abc)" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse something" $
      Wkt.parseString Line.multiLineString "multilinestring ((1.0 2.0, 2.0 3.0))" ^?! Trifecta._Success `shouldBe` GeoJSON.MultiLineStringGeometry [GeoJSON.LineStringGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 3.0]]]
    it "Parse spaces" $
      Wkt.parseString Line.multiLineString "multilinestring( ( 1.0 2.0 ,2.0 3.0 ) )" ^?! Trifecta._Success `shouldBe` GeoJSON.MultiLineStringGeometry [GeoJSON.LineStringGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 3.0]]]

testPolygons :: Spec
testPolygons =
  describe "simple polygons" $ do
    it "Parse empty" $
      Wkt.parseString Polygon.polygon "polygon empty" ^?! Trifecta._Success `shouldBe` Polygon.emptyPolygon
    it "Parse something" $
      Wkt.parseString Polygon.polygon "polygon ((1.0 2.0, 2.0 3.0))" ^?! Trifecta._Success `shouldBe` GeoJSON.PolygonGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 3.0]] []
    it "Parse spaces" $
      Wkt.parseString Polygon.polygon "polygon( (1.0 2.0,2.0 3.0) )" ^?! Trifecta._Success `shouldBe` GeoJSON.PolygonGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 3.0]] []
    it "Parse something with hole" $
      Wkt.parseString Polygon.polygon "polygon ((1.0 2.0, 2.0 3.0), (1.1 1.9))" ^?! Trifecta._Success `shouldBe` GeoJSON.PolygonGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 3.0]] [[GeoJSON.PointGeometry [1.1, 1.9]]]
    it "Parse something with hole spaces" $
      Wkt.parseString Polygon.polygon "polygon( ( 1.0 2.0,2.0 3.0 ) ,( 1.1 1.9 ) )" ^?! Trifecta._Success `shouldBe` GeoJSON.PolygonGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 3.0]] [[GeoJSON.PointGeometry [1.1, 1.9]]]

testMultiPolygons :: Spec
testMultiPolygons =
  describe "simple multipolygons" $ do
    it "Parse empty" $
      Wkt.parseString Polygon.multiPolygon "multipolygon empty" ^?! Trifecta._Success `shouldBe` Polygon.emptyMultiPolygon
    it "Parse something" $
      Wkt.parseString Polygon.multiPolygon "multipolygon (((1.0 2.0, 2.0 3.0)))" ^?! Trifecta._Success `shouldBe` GeoJSON.MultiPolygonGeometry [GeoJSON.PolygonGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 3.0]] []]
    it "Parse something with hole" $
      Wkt.parseString Polygon.multiPolygon "multipolygon ( ((1.0 2.0, 2.0 3.0), (1.1 1.9) ), ((10 10, 10 20), (60 60, 70 70 ) ))" ^?! Trifecta._Success `shouldBe` GeoJSON.MultiPolygonGeometry [GeoJSON.PolygonGeometry [GeoJSON.PointGeometry [1.0, 2.0], GeoJSON.PointGeometry [2.0, 3.0]] [[GeoJSON.PointGeometry [1.1, 1.9]]], GeoJSON.PolygonGeometry [GeoJSON.PointGeometry [10.0, 10.0], GeoJSON.PointGeometry [10.0, 20.0]] [[GeoJSON.PointGeometry [60, 60], GeoJSON.PointGeometry [70, 70]]]]

testGeometryCollection :: Spec
testGeometryCollection =
  describe "simple geometry collections" $ do
    it "Parse empty" $
      Wkt.parseString GeometryCollection.geometryCollection "geometrycollection empty" ^?! Trifecta._Success `shouldBe` GeometryCollection.emptyGeometryCollection
    it "Parse something" $
      Wkt.parseString GeometryCollection.geometryCollection "GeometryCollection(POINT (10 10),POINT (30 30),LINESTRING (15 15, 20 20))" ^?! Trifecta._Success `shouldBe` [GeoJSON.Point (GeoJSON.PointGeometry [10.0, 10.0]), GeoJSON.Point (GeoJSON.PointGeometry [30.0, 30.0]), GeoJSON.LineString(GeoJSON.LineStringGeometry [GeoJSON.PointGeometry [15.0, 15.0], GeoJSON.PointGeometry [20.0, 20.0]]) ]
    it "Parse something with spaces" $
      Wkt.parseString GeometryCollection.geometryCollection "GeometryCollection( POINT(10 10), POINT(30 30), LINESTRING( 15 15, 20 20 ) )" ^?! Trifecta._Success `shouldBe` [GeoJSON.Point (GeoJSON.PointGeometry [10.0, 10.0]), GeoJSON.Point (GeoJSON.PointGeometry [30.0, 30.0]), GeoJSON.LineString(GeoJSON.LineStringGeometry [GeoJSON.PointGeometry [15.0, 15.0], GeoJSON.PointGeometry [20.0, 20.0]]) ]
