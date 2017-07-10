module Wkt where

import           Control.Applicative
import qualified Data.ByteString      as B8
import           Data.ByteString.UTF8 as UTF8
import           Data.Char
import           Data.Scientific
import           Text.Trifecta
import           Text.Trifecta.Delta


delta :: String -> Delta
delta str = Directed (UTF8.fromString str) 0 0 0 0

parseByteString :: Parser a -> ByteString -> Result a
parseByteString p bs = Text.Trifecta.parseByteString p (Directed lowerBs 0 0 0 0) lowerBs
  where
    lowerBs = d8ToLower bs
    d8ToLower = B8.map f
      where
        f w | w >= 65 && w <= 90 = w + 32
            | otherwise = w

parseString :: Parser a -> String -> Result a
parseString p s = Text.Trifecta.parseString p (Wkt.delta lowerS) lowerS
  where
    lowerS = asciiToLower s
    asciiToLower = fmap f
      where
        f c | 'A' <= c && c <= 'Z' = chr (ord c + 32)
            | otherwise = c


emptySet :: Parser [a]
emptySet = do
  _ <- string "empty"
  pure []

emptySets :: Parser ([a], [[a]])
emptySets = do
  _ <- string "empty"
  pure ([], [])

number :: Parser Scientific
number = do
    sign <- (char '+' >> pure id) <|> (char '-' >> pure negate) <|> pure id
    n <- integerOrScientific
    pure (sign (either fromInteger id n))
