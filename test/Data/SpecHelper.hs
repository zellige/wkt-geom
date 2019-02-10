module Data.SpecHelper where

import qualified Data.Geospatial             as Geospatial
import qualified Data.LinearRing             as LinearRing
import qualified Data.LineString             as LineString
import qualified Data.Sequence               as Sequence
import qualified Data.Word                   as Word
import qualified HaskellWorks.Hspec.Hedgehog as HedgehogHspec
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Hspec                  (Spec, it)

import qualified Data.Ewkb                   as Ewkb
import qualified Data.Internal.Ewkb.Geometry as EwkbGeometry
import qualified Data.Internal.Wkb.Endian    as Endian
import qualified Data.Internal.Wkb.Geometry  as Geometry
import qualified Data.Wkb                    as Wkb

-- Helpers

type GeometryGenerator = Gen Geospatial.GeoPositionWithoutCRS -> Gen Geospatial.GeospatialGeometry

testRoundTripWkbGeometryParsing :: String -> GeometryGenerator -> Spec
testRoundTripWkbGeometryParsing name generator =
  mapM_ (testRoundTripGeometryParsing' roundTripWkb name generator) coordPointGenerators

testRoundTripEwkbGeometryParsing :: String -> GeometryGenerator -> Spec
testRoundTripEwkbGeometryParsing name generator =
  mapM_ (testRoundTripGeometryParsing' roundTripEwkb name generator) coordPointGenerators

testRoundTripGeometryParsing' :: (Endian.EndianType -> Geospatial.GeospatialGeometry -> PropertyT IO ()) -> String -> GeometryGenerator -> (Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS) -> Spec
testRoundTripGeometryParsing' f name generator (coordType, genCoordPoint) =
  it ("round trips wkb " ++ name ++ ": " ++ show coordType) $
    HedgehogHspec.require $ property $ do
      geometry <- forAll $ generator genCoordPoint
      endianType <- forAll genEndianType
      f endianType geometry

roundTripWkb :: MonadTest m => Endian.EndianType -> Geospatial.GeospatialGeometry -> m ()
roundTripWkb endianType geometry =
  let
    p = Wkb.parseByteString . Wkb.toByteString endianType
  in
    p geometry === Right geometry

roundTripEwkb :: MonadTest m => Endian.EndianType -> Geospatial.GeospatialGeometry -> m ()
roundTripEwkb endianType geometry =
  let
    p = Ewkb.parseByteString . Ewkb.toByteString endianType (EwkbGeometry.Srid (4326 :: Word.Word32))
  in
    p geometry === Right geometry

-- Generators

upperBoundOfMultiGeometries :: Int
upperBoundOfMultiGeometries = 10

upperBoundOfPoints :: Int
upperBoundOfPoints = 100

coordPointGenerators :: [(Geometry.CoordinateType, Gen Geospatial.GeoPositionWithoutCRS)]
coordPointGenerators =
  [ (Geometry.TwoD, genCoordPointXY)
  , (Geometry.Z, genCoordPointXYZ)
-- We can't roundtrip or express M in the current geometry (hs-geojson)
--  , (Geometry.M, genCoordPointXYZ)
  , (Geometry.ZM, genCoordPointXYZM)
  ]

genGeometryCollection :: Gen Geospatial.GeoPositionWithoutCRS -> Gen Geospatial.GeospatialGeometry
genGeometryCollection genCoordPoint = do
  geoCollection <- Gen.seq (Range.linear 0 upperBoundOfMultiGeometries) (genGeospatialGeometry genCoordPoint)
  return $ Geospatial.Collection geoCollection

genGeospatialGeometry :: Gen Geospatial.GeoPositionWithoutCRS -> Gen Geospatial.GeospatialGeometry
genGeospatialGeometry genCoordPoint = Gen.recursive Gen.choice
  [ genPoint genCoordPoint
  , genLine genCoordPoint
  , genPolygon genCoordPoint
  , genMultiPoint genCoordPoint
  , genMultiLine genCoordPoint
  , genMultiPolygon genCoordPoint
  ]
  [ genGeometryCollection genCoordPoint ]

genMultiPolygon :: Gen Geospatial.GeoPositionWithoutCRS -> Gen Geospatial.GeospatialGeometry
genMultiPolygon genCoordPoint = do
  geoMultiPolygon <- Geospatial.GeoMultiPolygon <$> Gen.seq (Range.linear 0 upperBoundOfMultiGeometries) (genLinearRings genCoordPoint)
  return $ Geospatial.MultiPolygon geoMultiPolygon

genPolygon :: Gen Geospatial.GeoPositionWithoutCRS -> Gen Geospatial.GeospatialGeometry
genPolygon genCoordPoint = do
  geoPolygon <- Geospatial.GeoPolygon <$> genLinearRings genCoordPoint
  return $ Geospatial.Polygon geoPolygon

genMultiLine :: Gen Geospatial.GeoPositionWithoutCRS -> Gen Geospatial.GeospatialGeometry
genMultiLine genCoordPoint = do
  geoMultiLine <- Geospatial.GeoMultiLine <$> Gen.seq (Range.linear 0 upperBoundOfMultiGeometries) (genLineString genCoordPoint)
  return $ Geospatial.MultiLine geoMultiLine

genLine :: Gen Geospatial.GeoPositionWithoutCRS -> Gen Geospatial.GeospatialGeometry
genLine genCoordPoint = do
  geoLine <- Geospatial.GeoLine <$> genLineString genCoordPoint
  return $ Geospatial.Line geoLine

genMultiPoint :: Gen Geospatial.GeoPositionWithoutCRS -> Gen Geospatial.GeospatialGeometry
genMultiPoint genCoordPoint = do
  geoMultiPoint <- Geospatial.GeoMultiPoint <$> genCoordPoints genCoordPoint
  return $ Geospatial.MultiPoint geoMultiPoint

genPoint :: Gen Geospatial.GeoPositionWithoutCRS -> Gen Geospatial.GeospatialGeometry
genPoint genCoordPoint = do
  geoPoint <- Geospatial.GeoPoint <$> genCoordPoint
  return $ Geospatial.Point geoPoint

genLinearRings :: Gen Geospatial.GeoPositionWithoutCRS -> Gen (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
genLinearRings genCoordPoint =
  Gen.seq (Range.linear 0 upperBoundOfMultiGeometries) (genLinearRing genCoordPoint)

genLinearRing :: Gen Geospatial.GeoPositionWithoutCRS -> Gen (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
genLinearRing genCoordPoint =
  LinearRing.makeLinearRing <$> genCoordPoint <*> genCoordPoint <*> genCoordPoint <*> genCoordPoints genCoordPoint

genLineString :: Gen Geospatial.GeoPositionWithoutCRS -> Gen (LineString.LineString Geospatial.GeoPositionWithoutCRS)
genLineString genCoordPoint =
  LineString.makeLineString <$> genCoordPoint <*> genCoordPoint <*> genCoordPoints genCoordPoint

genCoordPoints :: Gen Geospatial.GeoPositionWithoutCRS -> Gen (Sequence.Seq Geospatial.GeoPositionWithoutCRS)
genCoordPoints =
  Gen.seq (Range.linear 0 upperBoundOfPoints)

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
genDouble = Gen.double $ Range.linearFrac (-10e6) 10e6

genEndianType :: Gen Endian.EndianType
genEndianType = Gen.choice
  [ Gen.constant Endian.BigEndian
  , Gen.constant Endian.LittleEndian
  ]


-- Example

point1 :: Geospatial.GeoPositionWithoutCRS
point1 = Geospatial.GeoPointXY (Geospatial.PointXY 1.0 2.0)

point2 :: Geospatial.GeoPositionWithoutCRS
point2 = Geospatial.GeoPointXY (Geospatial.PointXY 3.0 4.0)

lineString1 :: LineString.LineString Geospatial.GeoPositionWithoutCRS
lineString1 = LineString.makeLineString (Geospatial.GeoPointXY (Geospatial.PointXY 1.0 2.0)) (Geospatial.GeoPointXY (Geospatial.PointXY 3.0 4.0)) Sequence.empty

lineString2 :: LineString.LineString Geospatial.GeoPositionWithoutCRS
lineString2 = LineString.makeLineString (Geospatial.GeoPointXY (Geospatial.PointXY 1.5 2.5)) (Geospatial.GeoPointXY (Geospatial.PointXY 3.5 4.5)) Sequence.empty

lineString3 :: LineString.LineString Geospatial.GeoPositionWithoutCRS
lineString3 = LineString.makeLineString (Geospatial.GeoPointXY (Geospatial.PointXY 15 15)) (Geospatial.GeoPointXY (Geospatial.PointXY 20 20)) (Sequence.singleton (Geospatial.GeoPointXY (Geospatial.PointXY 20 20)))

lineString4 :: LineString.LineString Geospatial.GeoPositionWithoutCRS
lineString4 = LineString.makeLineString (Geospatial.GeoPointXY (Geospatial.PointXY 15 15)) (Geospatial.GeoPointXY (Geospatial.PointXY 20 20)) (Sequence.singleton (Geospatial.GeoPointXY (Geospatial.PointXY 25 25)))

linearRing1 :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
linearRing1 = LinearRing.makeLinearRing (Geospatial.GeoPointXY (Geospatial.PointXY 1.0 2.0)) (Geospatial.GeoPointXY (Geospatial.PointXY 3.0 4.0)) (Geospatial.GeoPointXY (Geospatial.PointXY 5.0 6.0)) Sequence.empty

linearRing2 :: LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS
linearRing2 = LinearRing.makeLinearRing (Geospatial.GeoPointXY (Geospatial.PointXY 1.5 2.5)) (Geospatial.GeoPointXY (Geospatial.PointXY 3.5 4.5)) (Geospatial.GeoPointXY (Geospatial.PointXY 5.5 6.5)) Sequence.empty
