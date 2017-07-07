module Points (
  emptyPoint
, point
, pointTaggedText
, multipointTaggedText
) where

import           Control.Applicative
import           Data.Geography.GeoJSON
import           Data.Scientific
import           Text.Trifecta

import           Wkt

pointTaggedText :: Parser PointGeometry
pointTaggedText = do
  _ <- string "point" <|> string "POINT"
  _ <- spaces
  x <- bracketedPoint <|> (string "empty" <|> string "EMPTY" >> pure emptyPoint)
  _ <- spaces
  pure x

multipointTaggedText :: Parser MultiPointGeometry
multipointTaggedText = do
  _ <- string "multipoint" <|> string "MULTIPOINT"
  _ <- spaces
  xl <- emptySet <|> manyPoints
  pure (MultiPointGeometry xl)

manyPoints :: Parser [PointGeometry]
manyPoints = do
  _ <- char '('
  xl <- justPoints <|> justBracketedPoints
  _ <- char ')'
  pure xl

justPoints :: Parser [PointGeometry]
justPoints = do
  x <- pointText
  xs <- many (char ',' >> spaces >> pointText)
  pure (x:xs)

justBracketedPoints :: Parser [PointGeometry]
justBracketedPoints = do
  x <- bracketedPoint
  xs <- many (char ',' >> spaces >> bracketedPoint)
  pure (x:xs)

bracketedPoint :: Parser PointGeometry
bracketedPoint = do
  _ <- char '('
  x <- pointText
  _ <- char ')'
  pure x

pointText :: Parser PointGeometry
pointText = PointGeometry <$> point

point :: Parser [Scientific]
point = do
  x <- number
  _ <- spaces
  y <- number
  pure [x, y]

emptyPoint :: PointGeometry
emptyPoint = PointGeometry []
