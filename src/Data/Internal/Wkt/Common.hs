module Data.Internal.Wkt.Common
  ( emptySet
  , emptySets
  , number
  ) where

import           Control.Applicative ((<|>))
import qualified Data.Scientific     as Scientific
import qualified Data.Sequence       as Sequence
import qualified Text.Trifecta       as Trifecta

emptySet :: Trifecta.Parser (Sequence.Seq a)
emptySet = do
  _ <- Trifecta.string "empty"
  pure Sequence.empty

emptySets :: Trifecta.Parser ([a], [[a]])
emptySets = do
  _ <- Trifecta.string "empty"
  pure ([], [])

number :: Trifecta.Parser Scientific.Scientific
number = do
    sign <- (Trifecta.char '+' >> pure id) <|> (Trifecta.char '-' >> pure negate) <|> pure id
    sign  . either fromInteger id <$> Trifecta.integerOrScientific

