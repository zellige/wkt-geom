module Data.Hex
  ( Hex
  , safeConvert
  ) where

import qualified Data.ByteString.Base16 as ByteStringBase16
import qualified Data.ByteString         as ByteString

newtype Hex = ByteString.ByteString

safeConvert :: ByteString.ByteString -> Maybe ByteString.ByteString
safeConvert byteString =
  let
    decoded = fst $ decode byteString
  in  
    if ByteString.null decoded then
      Nothing
    else
      Just decoded  
  
