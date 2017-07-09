module Line where

import           Control.Applicative
import           Data.Geography.GeoJSON
import           Text.Trifecta

import           Point
import           Wkt

lineString :: Parser LineStringGeometry
lineString = do
  _ <- string "linestring"
  _ <- spaces
  x <- emptySet <|> Line.lines
  pure (LineStringGeometry x)

multiLineString :: Parser MultiLineStringGeometry
multiLineString = do
  _ <- string "multilinestring"
  _ <- spaces
  x <- emptySet <|> manyLines
  pure (MultiLineStringGeometry x)

manyLines :: Parser [LineStringGeometry]
manyLines = do
  _ <- char '('
  x <- LineStringGeometry <$> Line.lines
  xs <- many (char ',' >> spaces >> LineStringGeometry <$> Line.lines)
  _ <-  char ')'
  pure (x:xs)

lines :: Parser [PointGeometry]
lines = do
  _ <- char '('
  x <- justPoints
  xs <- many commandPoint
  _ <- char ')'
  pure (PointGeometry x : xs)

commandPoint :: Parser PointGeometry
commandPoint = do
  _ <- char ','
  _ <- spaces
  x <- justPoints
  pure (PointGeometry x)

emptyLine :: LineStringGeometry
emptyLine = LineStringGeometry []
