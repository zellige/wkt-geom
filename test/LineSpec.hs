{-# LANGUAGE OverloadedStrings #-}

module LineSpec where

import           Control.Lens    ((^?), (^?!))
import qualified Data.Geospatial as Geospatial
import qualified Data.LineString as LineString
import qualified Data.Maybe      as Maybe
import           Test.Hspec      (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Text.Trifecta   as Trifecta

import qualified Line
import qualified Wkt

spec :: Spec
spec = do
  testLines
  testMultiLines

testLines :: Spec
testLines =
  describe "simple lines" $ do
    it "Parse incomplete" $
      Wkt.parseString Line.lineString "linestring" `shouldSatisfy` (Maybe.isJust . flip (^?) Trifecta._Failure)
    -- it "Parse empty" $
    --   Wkt.parseString Line.lineString "linestring empty" ^?! Trifecta._Success `shouldBe` Line.emptyLine
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
exampleLine =
  Geospatial.GeoLine $ LineString.makeLineString [1.0, 2.0] [1.0,2.5] [[1.0,3.0]]

exampleMultiLine :: Geospatial.GeoMultiLine
exampleMultiLine =
  Geospatial.mergeGeoLines [exampleLine]
