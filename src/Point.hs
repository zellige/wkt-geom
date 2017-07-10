module Point (
  emptyPoint
, emptyMultiPoint
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
  _ <- char '(' >> spaces
  xl <- unbracketedPoints <|> bracketedPoints
  _ <- spaces >> char ')' >> spaces
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
  _ <- spaces >> char '(' >> spaces
  x <- pointText
  _ <- spaces >> char ')' >> spaces
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

emptyMultiPoint :: MultiPointGeometry
emptyMultiPoint = MultiPointGeometry []
