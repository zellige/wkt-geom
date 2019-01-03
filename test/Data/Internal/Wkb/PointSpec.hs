{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.PointSpec where

import qualified Data.Binary.Get              as BinaryGet
import qualified Data.ByteString.Builder      as ByteStringBuilder
import qualified Data.Geospatial              as Geospatial
import qualified Data.Sequence                as Sequence
import qualified HaskellWorks.Hspec.Hedgehog  as HedgehogHspec
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range
import           Test.Hspec                   (Spec, describe, it)

import qualified Data.Internal.Wkb.EndianSpec as EndianSpec
import qualified Data.Internal.Wkb.Geometry   as Geometry
import qualified Data.Internal.Wkb.Point      as Point
import qualified Data.Wkb                     as Wkb

spec :: Spec
spec = do
  testCoordPointParsing
  testWkbPointParsing
  testWkbMultiPointParsing


-- Test Coord Point Parsing

testCoordPointParsing :: Spec
testCoordPointParsing =
  describe "Test coord point parsing" $
    mapM_ testCoordPointParsing' coordPointGenerators

testCoordPointParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testCoordPointParsing' (coordType, genCoordPoint) =
  it ("round trips coord point: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    coordPoint <- forAll genCoordPoint
    endianType <- forAll EndianSpec.genEndianType
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
    mapM_ testWkbPointParsing' coordPointGenerators

testWkbPointParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testWkbPointParsing' (coordType, genCoordPoint) =
  it ("round trips wkb point: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    point <- forAll $ Geospatial.GeoPoint <$> genCoordPoint
    endianType <- forAll EndianSpec.genEndianType
    roundTrip endianType point === (Right $ Geospatial.Point point)
  where roundTrip endianType =
          Wkb.parseByteString . ByteStringBuilder.toLazyByteString . Point.builderPoint endianType


-- Test Wkb MultiPoint Parsing

testWkbMultiPointParsing :: Spec
testWkbMultiPointParsing =
  describe "Test wkb multipoint parsing" $
    mapM_ testWkbMultiPointParsing' coordPointGenerators

testWkbMultiPointParsing' :: (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testWkbMultiPointParsing' (coordType, genCoordPoint) =
  it ("round trips wkb multipoint: " ++ show coordType) $ HedgehogHspec.require $ property $ do
    points <- forAll $ Geospatial.GeoMultiPoint <$> genCoordPoints genCoordPoint
    endianType <- forAll EndianSpec.genEndianType
    roundTrip endianType points === (Right $ Geospatial.MultiPoint points)
  where roundTrip endianType =
          Wkb.parseByteString . ByteStringBuilder.toLazyByteString . Point.builderMultiPoint endianType


-- Helpers

coordPointGenerators :: [(Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS)]
coordPointGenerators =
  [ (Geometry.TwoD, genCoordPointXY)
  , (Geometry.Z, genCoordPointXYZ)
  , (Geometry.M, genCoordPointXYZ)
  , (Geometry.ZM, genCoordPointXYZM)
  ]

genCoordPoints :: Gen Geospatial.GeoPositionWithoutCRS -> Gen (Sequence.Seq Geospatial.GeoPositionWithoutCRS)
genCoordPoints =
  Gen.seq (Range.linear 0 1000)

genCoordPointXY :: Gen Geospatial.GeoPositionWithoutCRS
genCoordPointXY = do
  point <- Geospatial.PointXY <$> genDouble <*> genDouble
  return $ Geospatial.GeoPointXY point

genCoordPointXYZ :: Gen Geospatial.GeoPositionWithoutCRS
genCoordPointXYZ = do
  point <- Geospatial.PointXYZ <$> genDouble <*> genDouble <*> genDouble
  return $ Geospatial.GeoPointXYZ point

genCoordPointXYZM :: Gen Geospatial.GeoPositionWithoutCRS
genCoordPointXYZM = do
  point <- Geospatial.PointXYZM <$> genDouble <*> genDouble <*> genDouble <*> genDouble
  return $ Geospatial.GeoPointXYZM point

genDouble :: Gen Double
genDouble = Gen.double $ Range.linearFrac (-10e12) 10e12
