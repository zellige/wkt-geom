{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkt.PolygonSpec where

import           Control.Lens    ((^?!))
import qualified Data.Geospatial as Geospatial
import qualified Data.LinearRing as LinearRing
import qualified Data.Sequence   as Sequence
import           Test.Hspec      (Spec, describe, it, shouldBe)
import qualified Text.Trifecta   as Trifecta

import qualified Data.Wkt        as Wkt

spec :: Spec
spec = do
  testPolygons
  testMultiPolygons

testPolygons :: Spec
testPolygons =
  describe "simple polygons" $ do
    it "Parse empty" $
      Wkt.parseString Wkt.polygon "polygon empty" ^?! Trifecta._Success `shouldBe` Wkt.emptyPolygon
    it "Parse something" $
      Wkt.parseString Wkt.polygon "polygon ((4.0 0.0, 0.0 4.0, -4.0 0.0, 0.0 -4.0, 4.0 0.0))" ^?! Trifecta._Success `shouldBe` examplePolygon
    it "Parse spaces" $
      Wkt.parseString Wkt.polygon "polygon ( (4.0 0.0, 0.0  4.0, -4.0 0.0 , 0.0  -4.0 , 4.0 0.0))" ^?! Trifecta._Success `shouldBe` examplePolygon
    it "Parse something with hole" $
      Wkt.parseString Wkt.polygon "polygon ((4.0 0.0, 0.0 4.0, -4.0 0.0, 0.0 -4.0, 4.0 0.0),(2.0 0.0, 0.0 2.0, -2.0 0.0, 0.0 -2.0, 2.0 0.0))" ^?! Trifecta._Success `shouldBe` examplePolygonWithHole
    it "Parse something with hole spaces" $
      Wkt.parseString Wkt.polygon "polygon ( (4.0 0.0, 0.0  4.0, -4.0 0.0 , 0.0  -4.0 , 4.0 0.0),(  2.0 0.0, 0.0  2.0 , -2.0 0.0, 0.0 -2.0, 2.0 0.0))" ^?! Trifecta._Success `shouldBe` examplePolygonWithHole
    it "Parse something with z" $
      Wkt.parseString Wkt.polygon "polygon z ((4.0 0.0 1.0, 0.0 4.0 1.0, -4.0 0.0 1.0, 0.0 -4.0 1.0, 4.0 0.0 1.0))" ^?! Trifecta._Success `shouldBe` examplePolygon3D
    it "Parse something with zm" $
      Wkt.parseString Wkt.polygon "polygon zm ((4.0 0.0 1.0 0.5, 0.0 4.0 1.0 0.5, -4.0 0.0 1.0 0.5, 0.0 -4.0 1.0 0.5, 4.0 0.0 1.0 0.5))" ^?! Trifecta._Success `shouldBe` examplePolygon4D


testMultiPolygons :: Spec
testMultiPolygons =
  describe "simple multipolygons" $ do
    it "Parse empty" $
      Wkt.parseString Wkt.multiPolygon "multipolygon empty" ^?! Trifecta._Success `shouldBe` Wkt.emptyMultiPolygon
    it "Parse something" $
      Wkt.parseString Wkt.multiPolygon "multipolygon (((4.0 0.0, 0.0 4.0, -4.0 0.0, 0.0 -4.0, 4.0 0.0)))" ^?! Trifecta._Success `shouldBe` exampleMultiPolygon
    it "Parse something with hole" $
      Wkt.parseString Wkt.multiPolygon "multipolygon (( (4.0 0.0, 0.0  4.0, -4.0 0.0 , 0.0  -4.0 , 4.0 0.0),(  2.0 0.0, 0.0  2.0 , -2.0 0.0, 0.0 -2.0, 2.0 0.0)))" ^?! Trifecta._Success `shouldBe` exampleMultiPolygonWithHole
    it "Parse something with z" $
      Wkt.parseString Wkt.multiPolygon "multipolygon z (((4.0 0.0 1.0, 0.0 4.0 1.0, -4.0 0.0 1.0, 0.0 -4.0 1.0, 4.0 0.0 1.0)))" ^?! Trifecta._Success `shouldBe` exampleMultiPolygon3D
    it "Parse something with zm" $
      Wkt.parseString Wkt.multiPolygon "multipolygon zm (((4.0 0.0 1.0 0.5, 0.0 4.0 1.0 0.5, -4.0 0.0 1.0 0.5, 0.0 -4.0 1.0 0.5, 4.0 0.0 1.0 0.5)))" ^?! Trifecta._Success `shouldBe` exampleMultiPolygon4D

examplePolygon :: Geospatial.GeoPolygon
examplePolygon =
  Geospatial.GeoPolygon (Sequence.singleton linearRingSingle)

examplePolygon3D :: Geospatial.GeoPolygon
examplePolygon3D =
  Geospatial.GeoPolygon (Sequence.singleton linearRingSingle3D)

examplePolygon4D :: Geospatial.GeoPolygon
examplePolygon4D =
  Geospatial.GeoPolygon (Sequence.singleton linearRingSingle4D)

examplePolygonWithHole :: Geospatial.GeoPolygon
examplePolygonWithHole = Geospatial.GeoPolygon linearRingDouble

linearRingSingle :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
linearRingSingle = LinearRing.makeLinearRing (Geospatial.GeoPointXY (Geospatial.PointXY 4.0 0.0)) (Geospatial.GeoPointXY (Geospatial.PointXY 0.0 4.0)) (Geospatial.GeoPointXY (Geospatial.PointXY (-4.0) 0.0)) (Sequence.fromList [Geospatial.GeoPointXY (Geospatial.PointXY 0.0 (-4.0))])

linearRingSingle3D :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
linearRingSingle3D = LinearRing.makeLinearRing (Geospatial.GeoPointXYZ (Geospatial.PointXYZ 4.0 0.0 1.0)) (Geospatial.GeoPointXYZ (Geospatial.PointXYZ 0.0 4.0 1.0)) (Geospatial.GeoPointXYZ (Geospatial.PointXYZ (-4.0) 0.0 1.0)) (Sequence.fromList [Geospatial.GeoPointXYZ (Geospatial.PointXYZ 0.0 (-4.0) 1.0)])

linearRingSingle4D :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
linearRingSingle4D = LinearRing.makeLinearRing (Geospatial.GeoPointXYZM (Geospatial.PointXYZM 4.0 0.0 1.0 0.5)) (Geospatial.GeoPointXYZM (Geospatial.PointXYZM 0.0 4.0 1.0 0.5)) (Geospatial.GeoPointXYZM (Geospatial.PointXYZM (-4.0) 0.0 1.0 0.5)) (Sequence.fromList [Geospatial.GeoPointXYZM (Geospatial.PointXYZM 0.0 (-4.0) 1.0 0.5)])

linearRingDouble :: Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
linearRingDouble = Sequence.fromList
  [ linearRingSingle
  , LinearRing.makeLinearRing (Geospatial.GeoPointXY (Geospatial.PointXY 2.0 0.0)) (Geospatial.GeoPointXY (Geospatial.PointXY 0.0 2.0)) (Geospatial.GeoPointXY (Geospatial.PointXY (-2.0) 0.0)) (Sequence.fromList [ Geospatial.GeoPointXY (Geospatial.PointXY 0.0 (-2.0))])
  ]

exampleMultiPolygon :: Geospatial.GeoMultiPolygon
exampleMultiPolygon = Geospatial.GeoMultiPolygon (Sequence.singleton (Sequence.singleton linearRingSingle))

exampleMultiPolygon3D :: Geospatial.GeoMultiPolygon
exampleMultiPolygon3D = Geospatial.GeoMultiPolygon (Sequence.singleton (Sequence.singleton linearRingSingle3D))

exampleMultiPolygon4D :: Geospatial.GeoMultiPolygon
exampleMultiPolygon4D = Geospatial.GeoMultiPolygon (Sequence.singleton (Sequence.singleton linearRingSingle4D))

exampleMultiPolygonWithHole :: Geospatial.GeoMultiPolygon
exampleMultiPolygonWithHole = Geospatial.GeoMultiPolygon (Sequence.singleton linearRingDouble)
