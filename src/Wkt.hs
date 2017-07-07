module Wkt where

import           Control.Applicative
import           Data.ByteString.UTF8 as UTF8
import           Data.Scientific
import           Text.Trifecta
import           Text.Trifecta.Delta

delta :: String -> Delta
delta str = Directed (UTF8.fromString str) 0 0 0 0

emptySet :: Parser [a]
emptySet = do
  _ <- string "empty" <|> string "EMPTY"
  pure []

emptySets :: Parser ([a], [[a]])
emptySets = do
  _ <- string "empty" <|> string "EMPTY"
  pure ([], [])

number :: Parser Scientific
number = do
    sign <- (char '+' >> pure id) <|> (char '-' >> pure negate) <|> pure id
    n <- integerOrScientific
    pure (sign (either fromInteger id n))
