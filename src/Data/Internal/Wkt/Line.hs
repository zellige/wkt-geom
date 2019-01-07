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
  (Trifecta.string "empty" >> pure emptyLine) <|> nonEmptyLines

nonEmptyLines :: Trifecta.Parser Geospatial.GeoLine
nonEmptyLines =
  (Trifecta.string "zm" >> Geospatial.GeoLine <$> line Point.justPointsXYZM)
  <|> (Trifecta.string "z" >> Geospatial.GeoLine <$> line Point.justPointsXYZ)
  <|> Geospatial.GeoLine <$> line Point.justPointsXY

multiLineString :: Trifecta.Parser Geospatial.GeoMultiLine
multiLineString = do
  _ <- Trifecta.string "multilinestring"
  _ <- Trifecta.spaces
  x <- Wkt.emptySet <|> nonEmptyMultiLines
  pure $ Geospatial.GeoMultiLine x

nonEmptyMultiLines :: Trifecta.Parser (Sequence.Seq (LineString.LineString Geospatial.GeoPositionWithoutCRS))
nonEmptyMultiLines =
  (Trifecta.string "zm" >> manyLines Point.justPointsXYZM)
  <|> (Trifecta.string "z" >> manyLines Point.justPointsXYZ)
  <|> manyLines Point.justPointsXY

manyLines :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS ->  Trifecta.Parser (Sequence.Seq (LineString.LineString Geospatial.GeoPositionWithoutCRS))
manyLines pointParser = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- line pointParser
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> line pointParser)
  _ <-  Trifecta.char ')' >> Trifecta.spaces
  pure $ x Sequence.:<| Sequence.fromList xs

line :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS -> Trifecta.Parser (LineString.LineString Geospatial.GeoPositionWithoutCRS)
line pointParser = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  first <- pointParser
  second <- commandPoint pointParser
  rest <- Trifecta.many (commandPoint pointParser)
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ LineString.makeLineString first second (Sequence.fromList rest)

commandPoint :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS -> Trifecta.Parser Geospatial.GeoPositionWithoutCRS
commandPoint pointParser = do
  _ <- Trifecta.char ','
  _ <- Trifecta.spaces
  pointParser

emptyLine :: Geospatial.GeoLine
emptyLine = Geospatial.GeoLine $ LineString.makeLineString Geospatial.GeoEmpty Geospatial.GeoEmpty Sequence.empty

emptyMultiLine :: Geospatial.GeoMultiLine
emptyMultiLine = Geospatial.mergeGeoLines Sequence.empty
