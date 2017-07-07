module Lines where

import           Control.Applicative
import           Data.Geography.GeoJSON
import           Text.Trifecta

import           Points
import           Wkt

commandLines :: Parser [PointGeometry]
commandLines = do
  _ <- char ','
  _ <- spaces
  x <- Lines.lines
  pure x

lineStringText :: Parser LineStringGeometry
lineStringText = do
  _ <- string "linestring" <|> string "LINESTRING"
  _ <- spaces
  x <- Lines.lines <|> emptySet
  pure (LineStringGeometry x)

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
