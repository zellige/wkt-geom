module Data.Wkt.Line where

import           Control.Applicative  ((<|>))
import qualified Data.Geospatial      as Geospatial
import qualified Data.LineString      as LineString
import qualified Data.Vector          as Vector
import qualified Data.Vector.Storable as VectorStorable
import qualified Text.Trifecta        as Trifecta

import qualified Data.Wkt             as Wkt
import qualified Data.Wkt.Point       as Point

lineString :: Trifecta.Parser Geospatial.GeoLine
lineString = do
  _ <- Trifecta.string "linestring"
  _ <- Trifecta.spaces
  (Trifecta.string "empty" >> pure emptyLine) <|> Geospatial.GeoLine <$> line

multiLineString :: Trifecta.Parser Geospatial.GeoMultiLine
multiLineString = do
  _ <- Trifecta.string "multilinestring"
  _ <- Trifecta.spaces
  x <- Wkt.emptySet <|> manyLines
  pure $ Geospatial.GeoMultiLine x

manyLines :: Trifecta.Parser (Vector.Vector (LineString.LineString Geospatial.GeoPositionWithoutCRS))
manyLines = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- line
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> line)
  _ <-  Trifecta.char ')' >> Trifecta.spaces
  pure $ Vector.cons x (Vector.fromList xs)

line :: Trifecta.Parser (LineString.LineString Geospatial.GeoPositionWithoutCRS)
line = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  first <- Point.justPoints
  second <- commandPoint
  rest <- Trifecta.many commandPoint
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ LineString.makeLineString first second (VectorStorable.fromList rest)

commandPoint :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS
commandPoint = do
  _ <- Trifecta.char ','
  _ <- Trifecta.spaces
  Point.justPoints

emptyLine :: Geospatial.GeoLine
emptyLine = Geospatial.GeoLine $ LineString.makeLineString Geospatial.GeoEmpty Geospatial.GeoEmpty VectorStorable.empty

emptyMultiLine :: Geospatial.GeoMultiLine
emptyMultiLine = Geospatial.mergeGeoLines Vector.empty
