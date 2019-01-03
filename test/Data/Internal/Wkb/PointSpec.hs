{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.PointSpec where

import qualified Data.Binary.Get             as BinaryGet
import qualified Data.ByteString.Builder     as ByteStringBuilder
import qualified Data.Geospatial             as Geospatial
import qualified HaskellWorks.Hspec.Hedgehog as HedgehogHspec
import           Hedgehog
import           Test.Hspec                  (Spec, describe, it)

import qualified Data.Internal.Wkb.Geometry  as Geometry
import qualified Data.Internal.Wkb.Point     as Point
import qualified Data.SpecHelper             as SpecHelper
import qualified Data.Wkb                    as Wkb

spec :: Spec
spec = do
  testCoordPointParsing
  testWkbPointParsing
  testWkbMultiPointParsing


-- Test Coord Point Parsing

testCoordPointParsing :: Spec
testCoordPointParsing =
  describe "Test coord point parsing" $
    mapM_ testCoordPointParsing' SpecHelper.coordPointGenerators

testCoordPointParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testCoordPointParsing' (coordType, genCoordPoint) =
  it ("round trips coord point: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    coordPoint <- forAll genCoordPoint
    endianType <- forAll SpecHelper.genEndianType
    roundTrip endianType coordPoint === coordPoint
  where
    roundTrip endianType coordPoint  =
      BinaryGet.runGet (Point.getCoordPoint endianType coordType) (encodedCoordPoint endianType coordPoint)
    encodedCoordPoint endianType coordPoint =
      ByteStringBuilder.toLazyByteString $ Point.builderCoordPoint endianType coordPoint


-- Test Wkb Point Parsing

testWkbPointParsing :: Spec
testWkbPointParsing =
  describe "Test wkb point parsing" $
    mapM_ testWkbPointParsing' SpecHelper.coordPointGenerators

testWkbPointParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testWkbPointParsing' (coordType, genCoordPoint) =
  it ("round trips wkb point: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    point <- forAll $ Geospatial.GeoPoint <$> genCoordPoint
    endianType <- forAll SpecHelper.genEndianType
    roundTrip endianType point === (Right $ Geospatial.Point point)
  where roundTrip endianType =
          Wkb.parseByteString . ByteStringBuilder.toLazyByteString . Point.builderPoint endianType


-- Test Wkb MultiPoint Parsing

testWkbMultiPointParsing :: Spec
testWkbMultiPointParsing =
  describe "Test wkb multipoint parsing" $
    mapM_ testWkbMultiPointParsing' SpecHelper.coordPointGenerators

testWkbMultiPointParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testWkbMultiPointParsing' (coordType, genCoordPoint) =
  it ("round trips wkb multipoint: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    points <- forAll $ Geospatial.GeoMultiPoint <$> SpecHelper.genCoordPoints genCoordPoint
    endianType <- forAll SpecHelper.genEndianType
    roundTrip endianType points === (Right $ Geospatial.MultiPoint points)
  where roundTrip endianType =
          Wkb.parseByteString . ByteStringBuilder.toLazyByteString . Point.builderMultiPoint endianType
