
{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.GeometryCollectionSpec where

import qualified Data.Geospatial             as Geospatial
import qualified HaskellWorks.Hspec.Hedgehog as HedgehogHspec
import           Hedgehog
import           Test.Hspec                  (Spec, describe, it)

import qualified Data.Internal.Wkb.Geometry  as Geometry
import qualified Data.SpecHelper             as SpecHelper

spec :: Spec
spec =
  testWkbGeometryCollectionParsing


-- Test Wkb GeometryCollection Parsing

testWkbGeometryCollectionParsing :: Spec
testWkbGeometryCollectionParsing =
  describe "Test wkb geometery collection parsing" $
    mapM_ testWkbGeometryCollectionParsing' SpecHelper.coordPointGenerators

testWkbGeometryCollectionParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testWkbGeometryCollectionParsing' (coordType, genCoordPoint) =
  it ("round trips wkb multipolygon: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    collection <- forAll $ SpecHelper.genGeometryCollection genCoordPoint
    endianType <- forAll SpecHelper.genEndianType
    SpecHelper.roundTripWkb endianType collection === Right collection
