module Polygon where

import           Control.Applicative
import           Data.Geography.GeoJSON
import           Text.Trifecta

import           Line
import           Wkt

polygon :: Parser PolygonGeometry
polygon = do
  _ <- string "polygon"
  _ <- spaces
  justPolygon

multiPolygon :: Parser MultiPolygonGeometry
multiPolygon = do
  _ <- string "multipolygon"
  _ <- spaces
  x <- emptySet <|> manyPolygons
  pure (MultiPolygonGeometry x)

manyPolygons :: Parser [PolygonGeometry]
manyPolygons = do
  _ <- char '('
  x <- justPolygon
  xs <- many (char ',' >> spaces >> polygon)
  _ <- char ')'
  pure (x:xs)

justPolygon :: Parser PolygonGeometry
justPolygon = do
  (e, h) <- Wkt.emptySets <|> exteriorAndholes
  pure (PolygonGeometry e h)

exteriorAndholes :: Parser ([PointGeometry], [[PointGeometry]])
exteriorAndholes = do
  _ <- char '('
  e <- Line.lines
  h <- many commandLines
  _ <- char ')'
  pure (e, h)

commandLines :: Parser [PointGeometry]
commandLines = do
  _ <- char ','
  _ <- spaces
  x <- Line.lines
  pure x

emptyPolygon :: PolygonGeometry
emptyPolygon = PolygonGeometry [] []
