{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkt.LineSpec where

import           Control.Lens    ((^?), (^?!))
import qualified Data.Geospatial as Geospatial
import qualified Data.LineString as LineString
import qualified Data.Maybe      as Maybe
import qualified Data.Sequence   as Sequence
import           Test.Hspec      (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Text.Trifecta   as Trifecta

import qualified Data.Wkt        as Wkt

spec :: Spec
spec = do
  testLines
  testMultiLines

testLines :: Spec
testLines =
  describe "simple lines" $ do
    it "Parse incomplete" $
      Wkt.parseString Wkt.lineString "linestring" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse empty" $
      Wkt.parseString Wkt.lineString "linestring empty" ^?! Trifecta._Success `shouldBe` Wkt.emptyLine
    it "Parse not points" $
      Wkt.parseString Wkt.lineString "linestring (abc)" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse something" $
      Wkt.parseString Wkt.lineString "linestring (1.0 2.0, 1.0 2.5, 1.0 3.0)" ^?! Trifecta._Success `shouldBe` exampleLine
    it "Parse spaces" $
      Wkt.parseString Wkt.lineString "linestring ( 1.0 2.0,1.0 2.5, 1.0  3.0)" ^?! Trifecta._Success `shouldBe` exampleLine
    it "Parse z lines" $
      Wkt.parseString Wkt.lineString "linestring z (1.0 2.0 3.0,1.0 2.5 4.0,1.0 3.0 5.0)" ^?! Trifecta._Success `shouldBe` example3DLine
    it "Parse z lines" $
      Wkt.parseString Wkt.lineString "linestring z(1.0 2.0 3.0,1.0 2.5 4.0,1.0 3.0 5.0)" ^?! Trifecta._Success `shouldBe` example3DLine
    it "Parse z lines" $
      Wkt.parseString Wkt.lineString "linestringz(1.0 2.0 3.0,1.0 2.5 4.0,1.0 3.0 5.0)" ^?! Trifecta._Success `shouldBe` example3DLine
    it "Parse zm lines" $
      Wkt.parseString Wkt.lineString "linestring zm (1.0 2.0 3.0 4.0,1.0 2.5 4.0 5.5, 1.0 3.0 5.0 7.0)" ^?! Trifecta._Success `shouldBe` example4DLine


testMultiLines :: Spec
testMultiLines =
  describe "simple multilines" $ do
    it "Parse incomplete" $
      Wkt.parseString Wkt.multiLineString "multilinestring" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse empty" $
      Wkt.parseString Wkt.multiLineString "multilinestring empty" ^?! Trifecta._Success `shouldBe` Wkt.emptyMultiLine
    it "Parse not points" $
      Wkt.parseString Wkt.multiLineString "multilinestring (abc)" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    it "Parse something" $
      Wkt.parseString Wkt.multiLineString "multilinestring ((1.0 2.0, 1.0 2.5, 1.0 3.0))" ^?! Trifecta._Success `shouldBe` exampleMultiLine
    it "Parse spaces" $
      Wkt.parseString Wkt.multiLineString "multilinestring ( ( 1.0 2.0,1.0 2.5, 1.0  3.0) )" ^?! Trifecta._Success `shouldBe` exampleMultiLine
    it "Parse z lines" $
      Wkt.parseString Wkt.multiLineString "multilinestring z ((1.0 2.0 3.0,1.0 2.5 4.0,1.0 3.0 5.0))" ^?! Trifecta._Success `shouldBe` example3DMultiLine
    it "Parse zm lines" $
      Wkt.parseString Wkt.multiLineString "multilinestring zm ((1.0 2.0 3.0 4.0,1.0 2.5 4.0 5.5, 1.0 3.0 5.0 7.0))" ^?! Trifecta._Success `shouldBe` example4DMultiLine

exampleLine :: Geospatial.GeoLine
exampleLine = Geospatial.GeoLine exampleLineString

example3DLine :: Geospatial.GeoLine
example3DLine = Geospatial.GeoLine exampleLine3DString

example4DLine :: Geospatial.GeoLine
example4DLine = Geospatial.GeoLine exampleLine4DString

exampleMultiLine :: Geospatial.GeoMultiLine
exampleMultiLine =  Geospatial.GeoMultiLine (Sequence.singleton exampleLineString)

example3DMultiLine :: Geospatial.GeoMultiLine
example3DMultiLine =  Geospatial.GeoMultiLine (Sequence.singleton exampleLine3DString)

example4DMultiLine :: Geospatial.GeoMultiLine
example4DMultiLine =  Geospatial.GeoMultiLine (Sequence.singleton exampleLine4DString)

exampleLineString :: LineString.LineString Geospatial.GeoPositionWithoutCRS
exampleLineString = LineString.makeLineString (Geospatial.GeoPointXY (Geospatial.PointXY 1.0 2.0)) (Geospatial.GeoPointXY (Geospatial.PointXY 1.0 2.5)) (Sequence.singleton (Geospatial.GeoPointXY (Geospatial.PointXY 1.0 3.0)))

exampleLine3DString :: LineString.LineString Geospatial.GeoPositionWithoutCRS
exampleLine3DString = LineString.makeLineString (Geospatial.GeoPointXYZ (Geospatial.PointXYZ 1.0 2.0 3.0)) (Geospatial.GeoPointXYZ (Geospatial.PointXYZ 1.0 2.5 4.0)) (Sequence.singleton (Geospatial.GeoPointXYZ (Geospatial.PointXYZ 1.0 3.0 5.0)))

exampleLine4DString :: LineString.LineString Geospatial.GeoPositionWithoutCRS
exampleLine4DString = LineString.makeLineString (Geospatial.GeoPointXYZM (Geospatial.PointXYZM 1.0 2.0 3.0 4.0)) (Geospatial.GeoPointXYZM (Geospatial.PointXYZM 1.0 2.5 4.0 5.5)) (Sequence.singleton (Geospatial.GeoPointXYZM (Geospatial.PointXYZM 1.0 3.0 5.0 7.0)))
