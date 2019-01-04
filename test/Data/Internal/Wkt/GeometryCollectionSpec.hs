{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkt.GeometryCollectionSpec where

import           Control.Lens    ((^?!))
import qualified Data.Geospatial as Geospatial
import qualified Data.Sequence   as Sequence
import           Test.Hspec      (Spec, describe, it, shouldBe)
import qualified Text.Trifecta   as Trifecta

import qualified Data.Wkt        as Wkt

import qualified Data.SpecHelper as SpecHelper

spec :: Spec
spec = testGeometryCollection

testGeometryCollection :: Spec
testGeometryCollection =
  describe "simple geometry collections" $ do
    it "Parse empty" $
      Wkt.parseString Wkt.geometryCollection "geometrycollection empty" ^?! Trifecta._Success `shouldBe` Wkt.emptyGeometryCollection
    it "Parse something" $
      Wkt.parseString Wkt.geometryCollection "GeometryCollection(POINT (1 2),POINT (3 4),LINESTRING (15 15, 20 20, 25 25))" ^?! Trifecta._Success `shouldBe` exampleGeometryCollection
    it "Parse something with spaces" $
      Wkt.parseString Wkt.geometryCollection "GeometryCollection ( POINT ( 1 2) , POINT (3 4) , LINESTRING (15 15  , 20 20, 25 25 ) ) " ^?! Trifecta._Success `shouldBe` exampleGeometryCollection

exampleGeometryCollection :: Sequence.Seq Geospatial.GeospatialGeometry
exampleGeometryCollection =
  Sequence.fromList
    [ Geospatial.Point $ Geospatial.GeoPoint SpecHelper.point1
    , Geospatial.Point $ Geospatial.GeoPoint SpecHelper.point2
    , Geospatial.Line $ Geospatial.GeoLine SpecHelper.lineString4
    ]
