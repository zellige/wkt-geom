module GeometryCollections where

import           Control.Applicative
import           Data.Geography.GeoJSON
import           Text.Trifecta

import           Lines
import           Points
import           Polygons
import           Wkt

geometryCollectionTaggedText :: Parser [Geometry]
geometryCollectionTaggedText = do
  _ <- string "multipoint" <|> string "MULTIPOINT"
  _ <- spaces
  x <- Wkt.emptySet <|> GeometryCollections.all
  pure x

all :: Parser [Geometry]
all = do
  let
    single = Point <$> pointTaggedText  <|> LineString <$> lineStringTaggedText <|> Polygon <$> polygonTaggedText
    multi = MultiPoint <$> multipointTaggedText <|> MultiLineString <$> multilineStringTaggedText <|> MultiPolygon <$> multipolygonTaggedText
  x <- single <|> multi
  xs <- many (char ',' >> spaces >> (single <|> multi))
  pure (x:xs)

emptyGeometryCollection :: [Geometry]
emptyGeometryCollection = []
