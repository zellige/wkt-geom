{-# LANGUAGE OverloadedStrings #-}

module Data.Wkt.LineSpec where

import           Control.Lens    ((^?), (^?!))
import qualified Data.Geospatial as Geospatial
import qualified Data.LineString as LineString
import qualified Data.Maybe      as Maybe
import qualified Data.Vector     as Vector
import           Test.Hspec      (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Text.Trifecta   as Trifecta

import qualified Data.Wkt        as Wkt
import qualified Data.Wkt.Line   as Line

spec :: Spec
spec = do
  testLines
  testMultiLines

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
      Wkt.parseString Line.lineString "linestring (1.0 2.0, 1.0 2.5, 1.0 3.0)" ^?! Trifecta._Success `shouldBe` exampleLine
    it "Parse spaces" $
      Wkt.parseString Line.lineString "linestring ( 1.0 2.0,1.0 2.5, 1.0  3.0)" ^?! Trifecta._Success `shouldBe` exampleLine

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
      Wkt.parseString Line.multiLineString "multilinestring ((1.0 2.0, 1.0 2.5, 1.0 3.0))" ^?! Trifecta._Success `shouldBe` exampleMultiLine
    it "Parse spaces" $
      Wkt.parseString Line.multiLineString "multilinestring ( ( 1.0 2.0,1.0 2.5, 1.0  3.0) )" ^?! Trifecta._Success `shouldBe` exampleMultiLine

exampleLine :: Geospatial.GeoLine
exampleLine = Geospatial.GeoLine exampleLineString

exampleMultiLine :: Geospatial.GeoMultiLine
exampleMultiLine =  Geospatial.GeoMultiLine (Vector.singleton exampleLineString)

exampleLineString :: LineString.LineString Geospatial.GeoPositionWithoutCRS
exampleLineString = LineString.makeLineString (Geospatial.GeoPointXY (Geospatial.PointXY 1.0 2.0)) (Geospatial.GeoPointXY (Geospatial.PointXY 1.0 2.5)) (Vector.singleton (Geospatial.GeoPointXY (Geospatial.PointXY 1.0 3.0)))
