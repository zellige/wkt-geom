module Data.Wkt.GeometryCollection where

import           Control.Applicative ((<|>))
import qualified Data.Geospatial     as Geospatial
import qualified Data.LinearRing     as LinearRing
import qualified Data.Vector         as Vector
import qualified Text.Trifecta       as Trifecta

import qualified Data.Wkt            as Wkt
import qualified Data.Wkt.Line       as Line
import qualified Data.Wkt.Point      as Point
import qualified Data.Wkt.Polygon    as Polygon

geometryCollection :: Trifecta.Parser (Vector.Vector Geospatial.GeospatialGeometry)
geometryCollection = do
  _ <- Trifecta.string "geometrycollection"
  _ <- Trifecta.spaces
  Wkt.emptySet <|> bracketedAll

bracketedAll :: Trifecta.Parser (Vector.Vector Geospatial.GeospatialGeometry)
bracketedAll = do
  _ <- Trifecta.char '(' >> Trifecta.spaces
  x <- parseAll
  _ <- Trifecta.spaces >> Trifecta.char ')'
  pure x

parseAll :: Trifecta.Parser (Vector.Vector Geospatial.GeospatialGeometry)
parseAll = do
  let
    single = Geospatial.Point <$> Point.point <|> Geospatial.Line <$> Line.lineString <|> Geospatial.Polygon <$> Polygon.polygon
    multi = Geospatial.MultiPoint <$> Point.multiPoint <|> Geospatial.MultiLine <$> Line.multiLineString <|> Geospatial.MultiPolygon <$> Polygon.multiPolygon
  x <- single <|> multi
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> (single <|> multi))
  pure $ Vector.cons x (Vector.fromList xs)

emptyGeometryCollection :: Vector.Vector Geospatial.GeospatialGeometry
emptyGeometryCollection = Vector.empty
