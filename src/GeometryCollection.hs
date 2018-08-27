module GeometryCollection where

import           Control.Applicative ((<|>))
import qualified Data.Geospatial     as Geospatial
import qualified Text.Trifecta       as Trifecta

import qualified Line
import qualified Point
import qualified Polygon
import qualified Wkt

geometryCollection :: Trifecta.Parser [Geospatial.GeospatialGeometry]
geometryCollection = do
  _ <- Trifecta.string "geometrycollection"
  _ <- Trifecta.spaces
  x <- Wkt.emptySet <|> bracketedAll
  pure x

bracketedAll :: Trifecta.Parser [Geospatial.GeospatialGeometry]
bracketedAll = do
  _ <- Trifecta.char '(' >> Trifecta.spaces
  x <- GeometryCollection.all
  _ <- Trifecta.spaces >> Trifecta.char ')'
  pure x

all :: Trifecta.Parser [Geospatial.GeospatialGeometry]
all = do
  let
    single = Geospatial.Point <$> Point.point <|> Geospatial.Line <$> Line.lineString <|> Geospatial.Polygon <$> Polygon.polygon
    multi = Geospatial.MultiPoint <$> Point.multiPoint <|> Geospatial.MultiLine <$> Line.multiLineString <|> Geospatial.MultiPolygon <$> Polygon.multiPolygon
  x <- single <|> multi
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> (single <|> multi))
  pure (x:xs)

emptyGeometryCollection :: [Geospatial.GeospatialGeometry]
emptyGeometryCollection = []
