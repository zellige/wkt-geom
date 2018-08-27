module Polygon where

import           Control.Applicative    ((<|>))
import qualified Data.Geography.GeoJSON as GeoJSON
import qualified Text.Trifecta          as Trifecta

import qualified Line
import qualified Wkt

polygon :: Trifecta.Parser GeoJSON.PolygonGeometry
polygon = do
  _ <- Trifecta.string "polygon"
  _ <- Trifecta.spaces
  justPolygon

multiPolygon :: Trifecta.Parser GeoJSON.MultiPolygonGeometry
multiPolygon = do
  _ <- Trifecta.string "multipolygon"
  _ <- Trifecta.spaces
  x <- Wkt.emptySet <|> manyPolygons
  pure (GeoJSON.MultiPolygonGeometry x)

manyPolygons :: Trifecta.Parser [GeoJSON.PolygonGeometry]
manyPolygons = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- justPolygon
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> justPolygon)
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure (x:xs)

justPolygon :: Trifecta.Parser GeoJSON.PolygonGeometry
justPolygon = do
  (e, h) <- Wkt.emptySets <|> exteriorAndholes
  pure (GeoJSON.PolygonGeometry e h)

exteriorAndholes :: Trifecta.Parser ([GeoJSON.PointGeometry], [[GeoJSON.PointGeometry]])
exteriorAndholes = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  e <- Line.lines
  h <- Trifecta.many commandLines
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure (e, h)

commandLines :: Trifecta.Parser [GeoJSON.PointGeometry]
commandLines = do
  _ <- Trifecta.char ','
  _ <- Trifecta.spaces
  x <- Line.lines
  pure x

emptyPolygon :: GeoJSON.PolygonGeometry
emptyPolygon = GeoJSON.PolygonGeometry [] []

emptyMultiPolygon :: GeoJSON.MultiPolygonGeometry
emptyMultiPolygon = GeoJSON.MultiPolygonGeometry []
