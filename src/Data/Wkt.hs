module Data.Wkt where

import           Control.Applicative  ((<|>))
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Char            as Char
import qualified Data.Scientific      as Scientific
import qualified Text.Trifecta        as Trifecta
import qualified Text.Trifecta.Delta  as TrifectaDelta


delta :: String -> TrifectaDelta.Delta
delta str = TrifectaDelta.Directed (UTF8.fromString str) 0 0 0 0

parseByteString :: Trifecta.Parser a -> ByteString.ByteString -> Trifecta.Result a
parseByteString p bs = Trifecta.parseByteString p (TrifectaDelta.Directed lowerBs 0 0 0 0) lowerBs
  where
    lowerBs = d8ToLower bs
    d8ToLower = ByteString.map f
      where
        f w | w >= 65 && w <= 90 = w + 32
            | otherwise = w

parseString :: Trifecta.Parser a -> String -> Trifecta.Result a
parseString p s = Trifecta.parseString p (Data.Wkt.delta lowerS) lowerS
  where
    lowerS = asciiToLower s
    asciiToLower = fmap f
      where
        f c | Char.isAsciiUpper c = Char.chr (Char.ord c + 32)
            | otherwise = c


emptySet :: Trifecta.Parser [a]
emptySet = do
  _ <- Trifecta.string "empty"
  pure []

emptySets :: Trifecta.Parser ([a], [[a]])
emptySets = do
  _ <- Trifecta.string "empty"
  pure ([], [])

number :: Trifecta.Parser Scientific.Scientific
number = do
    sign <- (Trifecta.char '+' >> pure id) <|> (Trifecta.char '-' >> pure negate) <|> pure id
    n <- Trifecta.integerOrScientific
    pure (sign (either fromInteger id n))
