{-# LANGUAGE OverloadedStrings #-}

module PointSpec where

import           Control.Lens    ((^?), (^?!))
import qualified Data.Geospatial as Geospatial
import qualified Data.Maybe      as Maybe
import           Test.Hspec      (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Text.Trifecta   as Trifecta

import qualified Point
import qualified Wkt

spec :: Spec
spec = do
  testPoints
  testMultiPoints

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
      Wkt.parseString Point.point "point (1.0 2.0)" ^?! Trifecta._Success `shouldBe` examplePoint
    it "Parse spaces" $
      Wkt.parseString Point.point "point( 1.0 2.0 )" ^?! Trifecta._Success `shouldBe` examplePoint

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
      Wkt.parseString Point.multiPoint "multipoint (1.0 2.0, 2.0 2.0)" ^?! Trifecta._Success `shouldBe` exampleMultiPoint
    it "Parse unbracketed spaces" $
      Wkt.parseString Point.multiPoint "multipoint( 1.0 2.0,2.0 2.0 )" ^?! Trifecta._Success `shouldBe` exampleMultiPoint
    it "Parse bracketed" $
      Wkt.parseString Point.multiPoint "multipoint ((1.0 2.0), (2.0 2.0))" ^?! Trifecta._Success `shouldBe` exampleMultiPoint
    it "Parse bracketed spaces" $
      Wkt.parseString Point.multiPoint "multipoint( ( 1.0 2.0) ,( 2.0 2.0) )" ^?! Trifecta._Success `shouldBe` exampleMultiPoint

examplePoint :: Geospatial.GeoPoint
examplePoint =
  Geospatial.GeoPoint [1.0, 2.0]

exampleMultiPoint :: Geospatial.GeoMultiPoint
exampleMultiPoint =
  Geospatial.mergeGeoPoints [Geospatial.GeoPoint [1.0, 2.0], Geospatial.GeoPoint [2.0, 2.0]]
