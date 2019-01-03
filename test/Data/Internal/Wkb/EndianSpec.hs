{-# LANGUAGE OverloadedStrings #-}

module Data.Internal.Wkb.EndianSpec where

import qualified Data.Binary.Get          as BinaryGet
import qualified Data.ByteString.Builder  as ByteStringBuilder
import           Test.Hspec               (Spec, describe, it, shouldBe)

import qualified Data.Internal.Wkb.Endian as Endian

spec :: Spec
spec =
  testGetEndian

testGetEndian :: Spec
testGetEndian =
  describe "get endian" $ do
    it "Returns BigEndian for 0" $
      roundTrip Endian.BigEndian `shouldBe` Endian.BigEndian
    it "Returns LittleEndian for 1" $
      roundTrip Endian.LittleEndian `shouldBe` Endian.LittleEndian
    it "Returns fail for other" $
      BinaryGet.runGetOrFail Endian.getEndianType (ByteStringBuilder.toLazyByteString $ ByteStringBuilder.word8 5) `shouldBe` Left ("", 1, "Invalid EndianType")

roundTrip :: Endian.EndianType -> Endian.EndianType
roundTrip endianType =
  BinaryGet.runGet Endian.getEndianType encodedEndianType
  where encodedEndianType = ByteStringBuilder.toLazyByteString $ Endian.builderEndianType endianType
