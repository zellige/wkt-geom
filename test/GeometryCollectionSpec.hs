{-# LANGUAGE OverloadedStrings #-}

module GeometryCollectionSpec where

import           Control.Lens       ((^?!))
import qualified Data.Geospatial    as Geospatial
import qualified Data.LineString    as LineString
import           Test.Hspec         (Spec, describe, it, shouldBe)
import qualified Text.Trifecta      as Trifecta

import qualified GeometryCollection
import qualified Wkt

spec :: Spec
spec = do
  testGeometryCollection

testGeometryCollection :: Spec
testGeometryCollection =
  describe "simple geometry collections" $ do
    it "Parse empty" $
      Wkt.parseString GeometryCollection.geometryCollection "geometrycollection empty" ^?! Trifecta._Success `shouldBe` GeometryCollection.emptyGeometryCollection
    it "Parse something" $
      Wkt.parseString GeometryCollection.geometryCollection "GeometryCollection(POINT (10 10),POINT (30 30),LINESTRING (15 15, 20 20, 25 25))" ^?! Trifecta._Success `shouldBe` exampleGeometryCollection
    it "Parse something with spaces" $
      Wkt.parseString GeometryCollection.geometryCollection "GeometryCollection ( POINT ( 10 10) , POINT (30 30) , LINESTRING (15 15  , 20 20, 25 25 ) ) " ^?! Trifecta._Success `shouldBe` exampleGeometryCollection

exampleGeometryCollection :: [Geospatial.GeospatialGeometry]
exampleGeometryCollection =
  [ Geospatial.Point $ Geospatial.GeoPoint [10, 10]
  , Geospatial.Point $ Geospatial.GeoPoint [30, 30]
  , Geospatial.Line $ Geospatial.GeoLine $ LineString.makeLineString [15, 15] [20, 20] [[25, 25]]
  ]
