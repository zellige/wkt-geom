module Wkt where

import           Control.Applicative
import           Data.ByteString.UTF8 as UTF8
import           Data.Char
import           Data.Scientific
import           Text.Trifecta
import           Text.Trifecta.Delta

delta :: String -> Delta
delta str = Directed (UTF8.fromString str) 0 0 0 0

parseByteString :: ByteString -> Parser ByteString
parseByteString _ = undefined
  -- string_ (stringSuspended lower) lower s
  -- where lower = B8.map toLower

-- parseString :: Parser a -> Delta -> String -> Result a

parseString :: Parser a -> String -> Result a
parseString p s = Text.Trifecta.parseString p (Wkt.delta lowerS) lowerS
  where
    lowerS = asciiToLower s
    asciiToLower = fmap f
      where
        offset = ord 'a' - ord 'A'
        f c | 'A' <= c && c <= 'Z' = chr (ord c + offset)
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
