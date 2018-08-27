module Line where

import           Control.Applicative ((<|>))
import qualified Data.Geospatial     as Geospatial
import qualified Text.Trifecta       as Trifecta

import qualified Point
import qualified Wkt

lineString :: Trifecta.Parser GeoJSON.LineStringGeometry
lineString = do
  _ <- Trifecta.string "linestring"
  _ <- Trifecta.spaces
  x <- Wkt.emptySet <|> Line.lines
  pure (GeoJSON.LineStringGeometry x)

multiLineString :: Trifecta.Parser GeoJSON.MultiLineStringGeometry
multiLineString = do
  _ <- Trifecta.string "multilinestring"
  _ <- Trifecta.spaces
  x <- Wkt.emptySet <|> manyLines
  pure (GeoJSON.MultiLineStringGeometry x)

manyLines :: Trifecta.Parser [GeoJSON.LineStringGeometry]
manyLines = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- GeoJSON.LineStringGeometry <$> Line.lines
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> GeoJSON.LineStringGeometry <$> Line.lines)
  _ <-  Trifecta.char ')' >> Trifecta.spaces
  pure (x:xs)

lines :: Trifecta.Parser [GeoJSON.PointGeometry]
lines = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  x <- Point.justPoints
  xs <- Trifecta.many commandPoint
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure (GeoJSON.PointGeometry x : xs)

commandPoint :: Trifecta.Parser GeoJSON.PointGeometry
commandPoint = do
  _ <- Trifecta.char ','
  _ <- Trifecta.spaces
  x <- Point.justPoints
  pure (GeoJSON.PointGeometry x)

emptyLine :: GeoJSON.LineStringGeometry
emptyLine = GeoJSON.LineStringGeometry []

emptyMultiLine :: GeoJSON.MultiLineStringGeometry
emptyMultiLine = GeoJSON.MultiLineStringGeometry []
