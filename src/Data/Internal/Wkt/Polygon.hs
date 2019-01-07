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
  x <- Wkt.emptySet <|> polygon'
  pure $ Geospatial.GeoPolygon x

multiPolygon :: Trifecta.Parser Geospatial.GeoMultiPolygon
multiPolygon = do
  _ <- Trifecta.string "multipolygon"
  _ <- Trifecta.spaces
  xs <- Wkt.emptySet <|> multiPolygon'
  pure $ Geospatial.GeoMultiPolygon xs

polygon' :: Trifecta.Parser (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS))
polygon' = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- linearRing
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> linearRing)
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ x Sequence.:<| Sequence.fromList xs

multiPolygon' :: Trifecta.Parser (Sequence.Seq (Sequence.Seq (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)))
multiPolygon' = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- polygon'
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> polygon')
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ x Sequence.:<| Sequence.fromList xs

linearRing :: Trifecta.Parser (LinearRing.LinearRing Geospatial.GeoPositionWithoutCRS)
linearRing = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  first <- Point.justPointsXY
  second <- Line.commandPoint
  third <- Line.commandPoint
  rest <- Trifecta.many Line.commandPoint
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ LinearRing.makeLinearRing first second third (SeqHelper.sequenceHead $ Sequence.fromList rest)

emptyPolygon :: Geospatial.GeoPolygon
emptyPolygon = Geospatial.GeoPolygon Sequence.empty

emptyMultiPolygon :: Geospatial.GeoMultiPolygon
emptyMultiPolygon = Geospatial.mergeGeoPolygons Sequence.empty

