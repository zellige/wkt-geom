{-# LANGUAGE OverloadedStrings #-}

module Data.Wkb.EndianSpec where

import qualified Data.Binary.Get         as BinaryGet
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy    as LazyByteString
import qualified GHC.Word                as Word
import           Test.Hspec              (Spec, describe, it, shouldBe)

import qualified Data.Wkb.Endian         as Endian

spec :: Spec
spec =
  testGetEndian

testGetEndian :: Spec
testGetEndian =
  describe "get endian" $ do
    it "Returns BigEndian for 0" $
      BinaryGet.runGet Endian.getEndianType (getByteString 0) `shouldBe` Endian.BigEndian
    it "Returns LittleEndian for 1" $
      BinaryGet.runGet Endian.getEndianType (getByteString 1) `shouldBe` Endian.LittleEndian
    it "Returns fail for other" $
      BinaryGet.runGetOrFail Endian.getEndianType (getByteString 5) `shouldBe` Left ("", 1, "Invalid EndianType")

getByteString :: Word.Word8 -> LazyByteString.ByteString
getByteString int =
  ByteStringBuilder.toLazyByteString $ ByteStringBuilder.word8 int
