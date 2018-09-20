{-# LANGUAGE OverloadedStrings #-}

module Data.Wkt.PolygonSpec where

import           Control.Lens         ((^?!))
import qualified Data.Geospatial      as Geospatial
import qualified Data.LinearRing      as LinearRing
import qualified Data.Vector          as Vector
import qualified Data.Vector.Storable as VectorStorable
import           Test.Hspec           (Spec, describe, it, shouldBe)
import qualified Text.Trifecta        as Trifecta

import qualified Data.Wkt             as Wkt
import qualified Data.Wkt.Polygon     as Polygon

spec :: Spec
spec = do
  testPolygons
  testMultiPolygons

testPolygons :: Spec
testPolygons =
  describe "simple polygons" $ do
    it "Parse empty" $
      Wkt.parseString Polygon.polygon "polygon empty" ^?! Trifecta._Success `shouldBe` Polygon.emptyPolygon
    it "Parse something" $
      Wkt.parseString Polygon.polygon "polygon ((4.0 0.0, 0.0 4.0, -4.0 0.0, 0.0 -4.0, 4.0 0.0))" ^?! Trifecta._Success `shouldBe` examplePolygon
    it "Parse spaces" $
      Wkt.parseString Polygon.polygon "polygon ( (4.0 0.0, 0.0  4.0, -4.0 0.0 , 0.0  -4.0 , 4.0 0.0))" ^?! Trifecta._Success `shouldBe` examplePolygon
    it "Parse something with hole" $
      Wkt.parseString Polygon.polygon "polygon ((4.0 0.0, 0.0 4.0, -4.0 0.0, 0.0 -4.0, 4.0 0.0),(2.0 0.0, 0.0 2.0, -2.0 0.0, 0.0 -2.0, 2.0 0.0))" ^?! Trifecta._Success `shouldBe` examplePolygonWithHole
    it "Parse something with hole spaces" $
      Wkt.parseString Polygon.polygon "polygon ( (4.0 0.0, 0.0  4.0, -4.0 0.0 , 0.0  -4.0 , 4.0 0.0),(  2.0 0.0, 0.0  2.0 , -2.0 0.0, 0.0 -2.0, 2.0 0.0))" ^?! Trifecta._Success `shouldBe` examplePolygonWithHole

testMultiPolygons :: Spec
testMultiPolygons =
  describe "simple multipolygons" $ do
    it "Parse empty" $
      Wkt.parseString Polygon.multiPolygon "multipolygon empty" ^?! Trifecta._Success `shouldBe` Polygon.emptyMultiPolygon
    it "Parse something" $
      Wkt.parseString Polygon.multiPolygon "multipolygon (((4.0 0.0, 0.0 4.0, -4.0 0.0, 0.0 -4.0, 4.0 0.0)))" ^?! Trifecta._Success `shouldBe` exampleMultiPolygon
    it "Parse something with hole" $
      Wkt.parseString Polygon.multiPolygon "multipolygon (( (4.0 0.0, 0.0  4.0, -4.0 0.0 , 0.0  -4.0 , 4.0 0.0),(  2.0 0.0, 0.0  2.0 , -2.0 0.0, 0.0 -2.0, 2.0 0.0)))" ^?! Trifecta._Success `shouldBe` exampleMultiPolygonWithHole

examplePolygon :: Geospatial.GeoPolygon
examplePolygon =
  Geospatial.GeoPolygon (Vector.singleton linearRingSingle)

examplePolygonWithHole :: Geospatial.GeoPolygon
examplePolygonWithHole = Geospatial.GeoPolygon linearRingDouble

linearRingSingle :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
linearRingSingle = LinearRing.makeLinearRing (Geospatial.GeoPointXY (Geospatial.PointXY 4.0 0.0)) (Geospatial.GeoPointXY (Geospatial.PointXY 0.0 4.0)) (Geospatial.GeoPointXY (Geospatial.PointXY (-4.0) 0.0)) (VectorStorable.fromList [Geospatial.GeoPointXY (Geospatial.PointXY 0.0 (-4.0))])

linearRingDouble :: Vector.Vector (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
linearRingDouble = Vector.fromList
  [ linearRingSingle
  , LinearRing.makeLinearRing (Geospatial.GeoPointXY (Geospatial.PointXY 2.0 0.0)) (Geospatial.GeoPointXY (Geospatial.PointXY 0.0 2.0)) (Geospatial.GeoPointXY (Geospatial.PointXY (-2.0) 0.0)) (VectorStorable.fromList [ Geospatial.GeoPointXY (Geospatial.PointXY 0.0 (-2.0))])
  ]

exampleMultiPolygon :: Geospatial.GeoMultiPolygon
exampleMultiPolygon = Geospatial.GeoMultiPolygon (Vector.singleton (Vector.singleton linearRingSingle))

exampleMultiPolygonWithHole :: Geospatial.GeoMultiPolygon
exampleMultiPolygonWithHole = Geospatial.GeoMultiPolygon (Vector.singleton linearRingDouble)
