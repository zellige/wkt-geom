module GeometryCollection where

import           Control.Applicative
import           Data.Geography.GeoJSON
import           Text.Trifecta

import           Line
import           Point
import           Polygon
import           Wkt

geometryCollection :: Parser [Geometry]
geometryCollection = do
  _ <- string "geometrycollection"
  _ <- spaces
  x <- Wkt.emptySet <|> bracketedAll
  pure x

bracketedAll :: Parser [Geometry]
bracketedAll = do
  _ <- char '(' >> spaces
  x <- GeometryCollection.all
  _ <- spaces >> char ')'
  pure x

all :: Parser [Geometry]
all = do
  let
    single = Point <$> Point.point  <|> LineString <$> Line.lineString <|> Polygon <$> polygon
    multi = MultiPoint <$> multiPoint <|> MultiLineString <$> multiLineString <|> MultiPolygon <$> multiPolygon
  x <- single <|> multi
  xs <- many (char ',' >> spaces >> (single <|> multi))
  pure (x:xs)

emptyGeometryCollection :: [Geometry]
emptyGeometryCollection = []
