module Point (
  emptyPoint
, justPoints
, point
, multiPoint
) where

import           Control.Applicative
import           Data.Geography.GeoJSON
import           Data.Scientific
import           Text.Trifecta

import           Wkt

point :: Parser PointGeometry
point = do
  _ <- string "point"
  _ <- spaces
  x <- (string "empty" >> pure emptyPoint) <|> bracketedPoint
  pure x

multiPoint :: Parser MultiPointGeometry
multiPoint = do
  _ <- string "multipoint"
  _ <- spaces
  xl <- emptySet <|> manyPoints
  pure (MultiPointGeometry xl)

manyPoints :: Parser [PointGeometry]
manyPoints = do
  _ <- char '('
  xl <- unbracketedPoints <|> bracketedPoints
  _ <- char ')'
  pure xl

unbracketedPoints :: Parser [PointGeometry]
unbracketedPoints = do
  x <- pointText
  xs <- many (char ',' >> spaces >> pointText)
  pure (x:xs)

bracketedPoints :: Parser [PointGeometry]
bracketedPoints = do
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
pointText = PointGeometry <$> justPoints

justPoints :: Parser [Scientific]
justPoints = do
  x <- number
  _ <- spaces
  y <- number
  pure [x, y]

emptyPoint :: PointGeometry
emptyPoint = PointGeometry []
