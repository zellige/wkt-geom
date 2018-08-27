module GeometryCollection where

import           Control.Applicative    ((<|>))
import qualified Data.Geography.GeoJSON as GeoJSON
import qualified Text.Trifecta          as Trifecta

import qualified Line
import qualified Point
import qualified Polygon
import qualified Wkt

geometryCollection :: Trifecta.Parser [GeoJSON.Geometry]
geometryCollection = do
  _ <- Trifecta.string "geometrycollection"
  _ <- Trifecta.spaces
  x <- Wkt.emptySet <|> bracketedAll
  pure x

bracketedAll :: Trifecta.Parser [GeoJSON.Geometry]
bracketedAll = do
  _ <- Trifecta.char '(' >> Trifecta.spaces
  x <- GeometryCollection.all
  _ <- Trifecta.spaces >> Trifecta.char ')'
  pure x

all :: Trifecta.Parser [GeoJSON.Geometry]
all = do
  let
    single = GeoJSON.Point <$> Point.point  <|> GeoJSON.LineString <$> Line.lineString <|> GeoJSON.Polygon <$> Polygon.polygon
    multi = GeoJSON.MultiPoint <$> Point.multiPoint <|> GeoJSON.MultiLineString <$> Line.multiLineString <|> GeoJSON.MultiPolygon <$> Polygon.multiPolygon
  x <- single <|> multi
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> (single <|> multi))
  pure (x:xs)

emptyGeometryCollection :: [GeoJSON.Geometry]
emptyGeometryCollection = []
