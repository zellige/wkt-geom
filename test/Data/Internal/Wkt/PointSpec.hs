{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkt.PointSpec where

import           Control.Lens    ((^?), (^?!))
import qualified Data.Geospatial as Geospatial
import qualified Data.Maybe      as Maybe
import qualified Data.Sequence   as Sequence
import           Test.Hspec      (Spec, describe, expectationFailure, it,
                                  shouldBe, shouldSatisfy)
import qualified Text.Trifecta   as Trifecta

import qualified Data.Wkt        as Wkt

spec :: Spec
spec = do
  testPoints
  testMultiPoints

testPoints :: Spec
testPoints =
  describe "simple points" $ do
    it "Parse incomplete" $
      Wkt.parseString Wkt.point "point" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse empty" $
      Wkt.parseString Wkt.point "point empty" ^?! Trifecta._Success `shouldBe` Wkt.emptyPoint
    it "Parse not points" $
      Wkt.parseString Wkt.point "point (abc)" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse something" $ do
      --Wkt.parseString Wkt.point "point (1.0 2.0)" ^?! Trifecta._Success `shouldBe` examplePoint
      let x = Wkt.parseString Wkt.point "point (1.0 2.0)"
      case x of
        Trifecta.Success a -> a `shouldBe` examplePoint
        Trifecta.Failure f -> expectationFailure (show f)
    it "Parse spaces" $
      Wkt.parseString Wkt.point "point( 1.0 2.0 )" ^?! Trifecta._Success `shouldBe` examplePoint
    it "Parse z points" $
      Wkt.parseString Wkt.point "point z (1.0 2.0 3.0)" ^?! Trifecta._Success `shouldBe` example3DPoint
    it "Parse zm points" $
      Wkt.parseString Wkt.point "point zm (1.0 2.0 3.0 4.0)" ^?! Trifecta._Success `shouldBe` example4DPoint

testMultiPoints :: Spec
testMultiPoints =
  describe "simple multipoints" $ do
    it "Parse incomplete" $
      Wkt.parseString Wkt.multiPoint "multipoint" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse empty" $
      Wkt.parseString Wkt.multiPoint "multipoint empty" ^?! Trifecta._Success `shouldBe` Wkt.emptyMultiPoint
    it "Parse not points" $
      Wkt.parseString Wkt.multiPoint "multipoint (abc)" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse unbracketed" $
      Wkt.parseString Wkt.multiPoint "multipoint (1.0 2.0, 2.0 2.0)" ^?! Trifecta._Success `shouldBe` exampleMultiPoint
    it "Parse unbracketed spaces" $
      Wkt.parseString Wkt.multiPoint "multipoint( 1.0 2.0,2.0 2.0 )" ^?! Trifecta._Success `shouldBe` exampleMultiPoint
    it "Parse bracketed" $
      Wkt.parseString Wkt.multiPoint "multipoint ((1.0 2.0), (2.0 2.0))" ^?! Trifecta._Success `shouldBe` exampleMultiPoint
    it "Parse bracketed spaces" $
      Wkt.parseString Wkt.multiPoint "multipoint( ( 1.0 2.0) ,( 2.0 2.0) )" ^?! Trifecta._Success `shouldBe` exampleMultiPoint
    it "Parse z points" $
      Wkt.parseString Wkt.multiPoint "multipoint z ((1.0 2.0 3.0),(2.0 2.0 2.0))"  ^?! Trifecta._Success `shouldBe` exampleMulti3DPoint
    it "Parse zm points" $
      Wkt.parseString Wkt.multiPoint "multipoint zm ((1.0 2.0 3.0 4.0),(2.0 2.0 2.0 2.0))" ^?! Trifecta._Success `shouldBe` exampleMulti4DPoint

examplePoint :: Geospatial.GeoPoint
examplePoint = Geospatial.GeoPoint (Geospatial.GeoPointXY (Geospatial.PointXY 1.0 2.0))

example3DPoint :: Geospatial.GeoPoint
example3DPoint = Geospatial.GeoPoint (Geospatial.GeoPointXYZ (Geospatial.PointXYZ 1.0 2.0 3.0))

example4DPoint :: Geospatial.GeoPoint
example4DPoint = Geospatial.GeoPoint (Geospatial.GeoPointXYZM (Geospatial.PointXYZM 1.0 2.0 3.0 4.0))

exampleMultiPoint :: Geospatial.GeoMultiPoint
exampleMultiPoint = Geospatial.GeoMultiPoint (Sequence.fromList [Geospatial.GeoPointXY (Geospatial.PointXY 1.0 2.0), Geospatial.GeoPointXY (Geospatial.PointXY 2.0 2.0)])

exampleMulti3DPoint :: Geospatial.GeoMultiPoint
exampleMulti3DPoint = Geospatial.GeoMultiPoint (Sequence.fromList [Geospatial.GeoPointXYZ (Geospatial.PointXYZ 1.0 2.0 3.0), Geospatial.GeoPointXYZ (Geospatial.PointXYZ 2.0 2.0 2.0)])

exampleMulti4DPoint :: Geospatial.GeoMultiPoint
exampleMulti4DPoint = Geospatial.GeoMultiPoint (Sequence.fromList [Geospatial.GeoPointXYZM (Geospatial.PointXYZM 1.0 2.0 3.0 4.0), Geospatial.GeoPointXYZM (Geospatial.PointXYZM 2.0 2.0 2.0 2.0)])
