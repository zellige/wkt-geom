module Wkt where

import           Control.Applicative
import           Data.ByteString.UTF8   as UTF8
import           Data.Geography.GeoJSON
import           Data.Scientific
import           Text.Trifecta
import           Text.Trifecta.Delta


delta :: String -> Delta
delta str = Directed (UTF8.fromString str) 0 0 0 0

lineStringText :: Parser LineStringGeometry
lineStringText = do
  _ <- string "linestring" <|> string "LINESTRING"
  _ <- spaces
  x <- Wkt.lines <|> pure [emptyPoint]
  pure (LineStringGeometry x)

lines :: Parser [PointGeometry]
lines = do
  _ <- char '('
  x <- point
  xs <- many commandPoint
  _ <- char ')'
  pure (PointGeometry x : xs)

commandPoint :: Parser PointGeometry
commandPoint = do
  _ <- char ','
  _ <- spaces
  x <- point
  pure (PointGeometry x)

pointText :: Parser PointGeometry
pointText = do
  _ <- string "point" <|> string "POINT"
  _ <- spaces
  x <- Wkt.points <|> emptySet
  pure (PointGeometry x)

points :: Parser [Scientific]
points = do
  _ <- char '('
  x <- point
  _ <- char ')'
  pure x

point :: Parser [Scientific]
point = do
  x <- number
  _ <- spaces
  y <- number
  pure [x, y]

emptyLine :: LineStringGeometry
emptyLine = LineStringGeometry [emptyPoint]

emptyPoint :: PointGeometry
emptyPoint = PointGeometry []

emptySet :: Parser [Scientific]
emptySet = do
  _ <- string "empty" <|> string "EMPTY"
  pure []

number :: Parser Scientific
number = do
    sign <- (char '+' >> pure id) <|> (char '-' >> pure negate) <|> pure id
    n <- integerOrScientific
    pure (sign (either fromInteger id n))
