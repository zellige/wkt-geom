module Polygons where

import           Control.Applicative
import           Data.Geography.GeoJSON
import           Text.Trifecta

import           Lines
import           Wkt

polygonTaggedText :: Parser PolygonGeometry
polygonTaggedText = do
  _ <- string "polygon" <|> string "POLYGON"
  _ <- spaces
  polygon

multipolygonTaggedText :: Parser MultiPolygonGeometry
multipolygonTaggedText = do
  _ <- string "multipolygon" <|> string "MULTIPOLYGON"
  _ <- spaces
  x <- emptySet <|> manyPolygons
  pure (MultiPolygonGeometry x)

manyPolygons :: Parser [PolygonGeometry]
manyPolygons = do
  _ <- char '('
  x <- polygon
  xs <- many (char ',' >> spaces >> polygon)
  _ <- char ')'
  pure (x:xs)

polygon :: Parser PolygonGeometry
polygon = do
  (e, h) <- exteriorAndholes <|> Wkt.emptySets
  pure (PolygonGeometry e h)

exteriorAndholes :: Parser ([PointGeometry], [[PointGeometry]])
exteriorAndholes = do
  _ <- char '('
  e <- Lines.lines
  h <- many commandLines
  _ <- char ')'
  pure (e, h)

commandLines :: Parser [PointGeometry]
commandLines = do
  _ <- char ','
  _ <- spaces
  x <- Lines.lines
  pure x

emptyPolygon :: PolygonGeometry
emptyPolygon = PolygonGeometry [] []
