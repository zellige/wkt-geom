{-# LANGUAGE OverloadedStrings #-}

module Data.Wkt.GeometryCollectionSpec where

import           Control.Lens                ((^?!))
import qualified Data.Geospatial             as Geospatial
import qualified Data.LineString             as LineString
import qualified Data.Vector                 as Vector
import           Test.Hspec                  (Spec, describe, it, shouldBe)
import qualified Text.Trifecta               as Trifecta

import qualified Data.Wkt                    as Wkt
import qualified Data.Wkt.GeometryCollection as GeometryCollection

import qualified Data.SpecHelper             as SpecHelper

spec :: Spec
spec = testGeometryCollection

testGeometryCollection :: Spec
testGeometryCollection =
  describe "simple geometry collections" $ do
    it "Parse empty" $
      Wkt.parseString GeometryCollection.geometryCollection "geometrycollection empty" ^?! Trifecta._Success `shouldBe` GeometryCollection.emptyGeometryCollection
    it "Parse something" $
      Wkt.parseString GeometryCollection.geometryCollection "GeometryCollection(POINT (1 2),POINT (3 4),LINESTRING (15 15, 20 20, 25 25))" ^?! Trifecta._Success `shouldBe` exampleGeometryCollection
    it "Parse something with spaces" $
      Wkt.parseString GeometryCollection.geometryCollection "GeometryCollection ( POINT ( 10 10) , POINT (30 30) , LINESTRING (15 15  , 20 20, 25 25 ) ) " ^?! Trifecta._Success `shouldBe` exampleGeometryCollection

exampleGeometryCollection :: Vector.Vector Geospatial.GeospatialGeometry
exampleGeometryCollection =
  Vector.fromList
    [ Geospatial.Point $ Geospatial.GeoPoint SpecHelper.point1
    , Geospatial.Point $ Geospatial.GeoPoint SpecHelper.point2
    , Geospatial.Line $ Geospatial.GeoLine SpecHelper.lineString4
    ]
