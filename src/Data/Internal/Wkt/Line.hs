module Data.Internal.Wkt.Line
  ( line
  , lineString
  , multiLineString
  , emptyLine
  , emptyMultiLine
  , commandPoint
  ) where

import           Control.Applicative      ((<|>))
import qualified Data.Geospatial          as Geospatial
import qualified Data.LineString          as LineString
import qualified Data.Sequence            as Sequence
import qualified Text.Trifecta            as Trifecta

import qualified Data.Internal.Wkt.Common as Wkt
import qualified Data.Internal.Wkt.Point  as Point

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

manyLines :: Trifecta.Parser (Sequence.Seq (LineString.LineString Geospatial.GeoPositionWithoutCRS))
manyLines = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- line
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> line)
  _ <-  Trifecta.char ')' >> Trifecta.spaces
  pure $ x Sequence.:<| Sequence.fromList xs

line :: Trifecta.Parser (LineString.LineString Geospatial.GeoPositionWithoutCRS)
line = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  first <- Point.justPointsXY
  second <- commandPoint
  rest <- Trifecta.many commandPoint
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ LineString.makeLineString first second (Sequence.fromList rest)

commandPoint :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS
commandPoint = do
  _ <- Trifecta.char ','
  _ <- Trifecta.spaces
  Point.justPointsXY

emptyLine :: Geospatial.GeoLine
emptyLine = Geospatial.GeoLine $ LineString.makeLineString Geospatial.GeoEmpty Geospatial.GeoEmpty Sequence.empty

emptyMultiLine :: Geospatial.GeoMultiLine
emptyMultiLine = Geospatial.mergeGeoLines Sequence.empty
