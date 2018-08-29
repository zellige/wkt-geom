module Data.Wkt.Point where

import           Control.Applicative ((<|>))
import qualified Data.Geospatial     as Geospatial
import qualified Data.Scientific     as Scientific
import qualified Text.Trifecta       as Trifecta

import qualified Data.Wkt            as Wkt

point :: Trifecta.Parser Geospatial.GeoPoint
point = do
  _ <- Trifecta.string "point"
  _ <- Trifecta.spaces
  (Trifecta.string "empty" >> pure emptyPoint) <|> bracketedPoint

multiPoint :: Trifecta.Parser Geospatial.GeoMultiPoint
multiPoint = do
  _ <- Trifecta.string "multipoint"
  _ <- Trifecta.spaces
  xl <- Wkt.emptySet <|> manyPoints
  pure (Geospatial.mergeGeoPoints xl)

manyPoints :: Trifecta.Parser [Geospatial.GeoPoint]
manyPoints = do
  _ <- Trifecta.char '(' >> Trifecta.spaces
  xl <- unbracketedPoints <|> bracketedPoints
  _ <- Trifecta.spaces >> Trifecta.char ')' >> Trifecta.spaces
  pure xl

unbracketedPoints :: Trifecta.Parser [Geospatial.GeoPoint]
unbracketedPoints = do
  x <- pointText
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> pointText)
  pure (x:xs)

bracketedPoints :: Trifecta.Parser [Geospatial.GeoPoint]
bracketedPoints = do
  x <- bracketedPoint
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> bracketedPoint)
  pure (x:xs)

bracketedPoint :: Trifecta.Parser Geospatial.GeoPoint
bracketedPoint = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  x <- pointText
  _ <- Trifecta.spaces >> Trifecta.char ')' >> Trifecta.spaces
  pure x

pointText :: Trifecta.Parser Geospatial.GeoPoint
pointText = Geospatial.GeoPoint <$> justPoints

justPoints :: Trifecta.Parser [Double]
justPoints = do
  x <- Wkt.number
  _ <- Trifecta.spaces
  y <- Wkt.number
  pure $ map Scientific.toRealFloat [x, y]

emptyPoint :: Geospatial.GeoPoint
emptyPoint = Geospatial.GeoPoint []

emptyMultiPoint :: Geospatial.GeoMultiPoint
emptyMultiPoint = Geospatial.GeoMultiPoint []
