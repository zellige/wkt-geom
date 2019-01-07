module Data.Internal.Wkt.Polygon
  ( polygon
  , multiPolygon
  , emptyPolygon
  , emptyMultiPolygon
  ) where

import           Control.Applicative      ((<|>))
import qualified Data.Geospatial          as Geospatial
import qualified Data.LinearRing          as LinearRing
import qualified Data.Sequence            as Sequence
import qualified Text.Trifecta            as Trifecta

import qualified Data.Internal.Wkt.Common as Wkt
import qualified Data.Internal.Wkt.Line   as Line
import qualified Data.Internal.Wkt.Point  as Point
import qualified Data.SeqHelper           as SeqHelper

polygon :: Trifecta.Parser Geospatial.GeoPolygon
polygon = do
  _ <- Trifecta.string "polygon"
  _ <- Trifecta.spaces
  x <- Wkt.emptySet <|> nonEmptyPolygon
  pure $ Geospatial.GeoPolygon x

nonEmptyPolygon :: Trifecta.Parser (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
nonEmptyPolygon =
  (Trifecta.string "zm" >> polygon' Point.justPointsXYZM)
  <|> (Trifecta.string "z" >> polygon' Point.justPointsXYZ)
  <|> polygon' Point.justPointsXY

multiPolygon :: Trifecta.Parser Geospatial.GeoMultiPolygon
multiPolygon = do
  _ <- Trifecta.string "multipolygon"
  _ <- Trifecta.spaces
  xs <- Wkt.emptySet <|> nonEmptyMultiPolygon
  pure $ Geospatial.GeoMultiPolygon xs

nonEmptyMultiPolygon :: Trifecta.Parser (Sequence.Seq (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
nonEmptyMultiPolygon =
  (Trifecta.string "zm" >> multiPolygon' Point.justPointsXYZM)
  <|> (Trifecta.string "z" >> multiPolygon' Point.justPointsXYZ)
  <|> multiPolygon' Point.justPointsXY

polygon' :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS -> Trifecta.Parser (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
polygon' pointParser = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- linearRing pointParser
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> linearRing pointParser)
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ x Sequence.:<| Sequence.fromList xs

multiPolygon' :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS -> Trifecta.Parser (Sequence.Seq (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
multiPolygon' pointParser = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- polygon' pointParser
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> polygon' pointParser)
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ x Sequence.:<| Sequence.fromList xs

linearRing :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS -> Trifecta.Parser (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
linearRing pointParser = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  first <- pointParser
  second <- Line.commandPoint pointParser
  third <- Line.commandPoint pointParser
  rest <- Trifecta.many (Line.commandPoint pointParser)
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ LinearRing.makeLinearRing first second third (SeqHelper.sequenceHead $ Sequence.fromList rest)

emptyPolygon :: Geospatial.GeoPolygon
emptyPolygon = Geospatial.GeoPolygon Sequence.empty

emptyMultiPolygon :: Geospatial.GeoMultiPolygon
emptyMultiPolygon = Geospatial.mergeGeoPolygons Sequence.empty

