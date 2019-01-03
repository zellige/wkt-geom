{-# LANGUAGE OverloadedStrings #-}

module Data.WkbHexParsingSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Geospatial as Geospatial
import           Test.Hspec      (Spec, describe, it, shouldBe)

import qualified Data.Wkb        as Wkb

spec :: Spec
spec =
  testWkbHexParsing

testWkbHexParsing :: Spec
testWkbHexParsing =
  describe "Test hex encoded wkb point" $ do
    it "Parse valid hex wkb" $
      Wkb.parseHexByteString exampleHexPoint `shouldBe` Right expectedPoint
    it "Not parse valid hex but invalid wkb" $
      Wkb.parseHexByteString "deadbeef" `shouldBe` Left "Could not parse wkb: Invalid EndianType"
    it "Some valid, some invalid hex" $
      Wkb.parseHexByteString "deadfish" `shouldBe` Left "Invalid hex representation: deadfish"
    it "All bad" $
      Wkb.parseHexByteString "cowboyx" `shouldBe` Left "Invalid hex representation: cowboyx"

exampleHexPoint :: ByteString.ByteString
exampleHexPoint = "0101000000000000000000f03f0000000000000040"

expectedPoint :: Geospatial.GeospatialGeometry
expectedPoint = Geospatial.Point (Geospatial.GeoPoint (Geospatial.GeoPointXY (Geospatial.PointXY 1.0 2.0)))
