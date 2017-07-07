module Lines where

import           Control.Applicative
import           Data.Geography.GeoJSON
import           Text.Trifecta

import           Points
import           Wkt

lineStringTaggedText :: Parser LineStringGeometry
lineStringTaggedText = do
  _ <- string "linestring" <|> string "LINESTRING"
  _ <- spaces
  x <- emptySet <|> Lines.lines
  pure (LineStringGeometry x)

multilineStringTaggedText :: Parser MultiLineStringGeometry
multilineStringTaggedText = do
  _ <- string "multilinestring" <|> string "MULTILINESTRING"
  _ <- spaces
  x <- emptySet <|> manyLines
  pure (MultiLineStringGeometry x)

manyLines :: Parser [LineStringGeometry]
manyLines = do
  _ <- char '('
  x <- LineStringGeometry <$> Lines.lines
  xs <- many (char ',' >> spaces >> LineStringGeometry <$> Lines.lines)
  _ <-  char ')'
  pure (x:xs)

lines :: Parser [PointGeometry]
lines = do
  _ <- char '('
  x <- Points.point
  xs <- many commandPoint
  _ <- char ')'
  pure (PointGeometry x : xs)

commandPoint :: Parser PointGeometry
commandPoint = do
  _ <- char ','
  _ <- spaces
  x <- point
  pure (PointGeometry x)

emptyLine :: LineStringGeometry
emptyLine = LineStringGeometry []
