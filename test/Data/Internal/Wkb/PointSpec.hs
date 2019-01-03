{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.PointSpec where

import qualified Data.Binary.Get              as BinaryGet
import qualified Data.ByteString.Builder      as ByteStringBuilder
import qualified Data.ByteString.Lazy         as LazyByteString
import qualified Data.Geospatial              as Geospatial
import           Data.Monoid                  ((<>))
import qualified Data.Sequence                as Sequence
import qualified Data.SpecHelper              as SpecHelper
import qualified HaskellWorks.Hspec.Hedgehog  as HedgehogHspec
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range
import           Test.Hspec                   (Spec, describe, it, shouldBe)

import qualified Data.Internal.Wkb.EndianSpec as EndianSpec
import qualified Data.Internal.Wkb.Geometry   as Geometry
import qualified Data.Internal.Wkb.Point      as Point
import qualified Data.Wkb                     as Wkb

spec :: Spec
spec = do
  testCoordPointParsing
  testWkbPointParsing
  testWkbMultiPointParsing

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
  where
    roundTrip endianType point  =
      Wkb.parseByteString (ByteStringBuilder.toLazyByteString $ Point.builderPoint endianType point)

testWkbMultiPointParsing :: Spec
testWkbMultiPointParsing =
  describe "Test wkb multi point" $
    it "Parse valid wkb multi point" $
      Wkb.parseByteString exampleWkbMultiPoint `shouldBe` (Right $ Geospatial.MultiPoint (Geospatial.GeoMultiPoint (Sequence.fromList [SpecHelper.point1, SpecHelper.point2])))

exampleWkbMultiPoint :: LazyByteString.ByteString
exampleWkbMultiPoint =
  ByteStringBuilder.toLazyByteString $
    ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 4
    <> ByteStringBuilder.int32BE 2
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.doubleBE 1.0
    <> ByteStringBuilder.doubleBE 2.0
    <> ByteStringBuilder.word8 0
    <> ByteStringBuilder.int32BE 1
    <> ByteStringBuilder.doubleBE 3.0
    <> ByteStringBuilder.doubleBE 4.0

coordPointGenerators :: [(Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS)]
coordPointGenerators =
  [ (Geometry.TwoD, genCoordPointXY)
  , (Geometry.Z, genCoordPointXYZ)
  , (Geometry.M, genCoordPointXYZ)
  , (Geometry.ZM, genCoordPointXYZM)
  ]

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
