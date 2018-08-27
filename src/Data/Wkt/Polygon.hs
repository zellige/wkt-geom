module Data.Wkt.Polygon where

import           Control.Applicative ((<|>))
import qualified Data.Geospatial     as Geospatial
import qualified Data.LinearRing     as LinearRing
import qualified Text.Trifecta       as Trifecta

import qualified Data.Wkt            as Wkt
import qualified Data.Wkt.Line       as Line
import qualified Data.Wkt.Point      as Point

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
  pure $ Geospatial.mergeGeoPolygons $ map Geospatial.GeoPolygon xs

polygon' :: Trifecta.Parser [LinearRing.LinearRing [Double]]
polygon' = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- linearRing
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> linearRing)
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ x:xs

multiPolygon' :: Trifecta.Parser [[LinearRing.LinearRing [Double]]]
multiPolygon' = do
  _ <- Trifecta.spaces >> Trifecta.char '('
  x <- polygon'
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> polygon')
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ x:xs

linearRing :: Trifecta.Parser (LinearRing.LinearRing [Double])
linearRing = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  first <- Point.justPoints
  second <- Line.commandPoint
  third <- Line.commandPoint
  rest <- Trifecta.many Line.commandPoint
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ LinearRing.makeLinearRing first second third (init rest)

emptyPolygon :: Geospatial.GeoPolygon
emptyPolygon = Geospatial.GeoPolygon []

emptyMultiPolygon :: Geospatial.GeoMultiPolygon
emptyMultiPolygon = Geospatial.mergeGeoPolygons []
