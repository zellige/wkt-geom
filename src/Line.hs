module Line where

import qualified Data.Geospatial as Geospatial
import qualified Data.LineString as LineString
import qualified Text.Trifecta   as Trifecta

import qualified Point

lineString :: Trifecta.Parser Geospatial.GeoLine
lineString = do
  _ <- Trifecta.string "linestring"
  _ <- Trifecta.spaces
  x <- Line.line
  pure $ Geospatial.GeoLine x

multiLineString :: Trifecta.Parser Geospatial.GeoMultiLine
multiLineString = do
  _ <- Trifecta.string "multilinestring"
  _ <- Trifecta.spaces
  x <- manyLines
  pure $ Geospatial.mergeGeoLines x

manyLines :: Trifecta.Parser [Geospatial.GeoLine]
manyLines = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- Geospatial.GeoLine <$> Line.line
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> Geospatial.GeoLine <$> Line.line)
  _ <-  Trifecta.char ')' >> Trifecta.spaces
  pure $ x:xs

line :: Trifecta.Parser (LineString.LineString [Double])
line = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  first <- Point.justPoints
  second <- commandPoint
  rest <- Trifecta.many commandPoint
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ LineString.makeLineString first second rest

commandPoint :: Trifecta.Parser [Double]
commandPoint = do
  _ <- Trifecta.char ','
  _ <- Trifecta.spaces
  x <- Point.justPoints
  pure x
