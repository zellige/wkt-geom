module Data.Internal.Wkt.Common
  ( emptySet
  , emptySets
  , number
  ) where

import           Control.Applicative ((<|>))
import qualified Data.Scientific     as Scientific
import qualified Data.Vector         as Vector
import qualified Text.Trifecta       as Trifecta

emptySet :: Trifecta.Parser (Vector.Vector a)
emptySet = do
  _ <- Trifecta.string "empty"
  pure Vector.empty

emptySets :: Trifecta.Parser ([a], [[a]])
emptySets = do
  _ <- Trifecta.string "empty"
  pure ([], [])

number :: Trifecta.Parser Scientific.Scientific
number = do
    sign <- (Trifecta.char '+' >> pure id) <|> (Trifecta.char '-' >> pure negate) <|> pure id
    sign  . either fromInteger id <$> Trifecta.integerOrScientific

