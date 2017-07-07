module Polygons where

import           Control.Applicative
import           Data.Geography.GeoJSON
import           Text.Trifecta

import           Lines
import           Wkt

polygonText :: Parser PolygonGeometry
polygonText = do
  _ <- string "polygon" <|> string "POLYGON"
  _ <- spaces
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
