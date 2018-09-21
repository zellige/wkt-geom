module Data.Internal.Wkt.Point
  ( point
  , multiPoint
  , justPoints
  , emptyPoint
  , emptyMultiPoint
  ) where

import           Control.Applicative      ((<|>))
import qualified Data.Geospatial          as Geospatial
import qualified Data.Scientific          as Scientific
import qualified Data.Vector              as Vector
import qualified Text.Trifecta            as Trifecta

import qualified Data.Internal.Wkt.Common as Wkt

point :: Trifecta.Parser Geospatial.GeoPoint
point = do
  _ <- Trifecta.string "point"
  _ <- Trifecta.spaces
  (Trifecta.string "empty" >> pure emptyPoint) <|> fmap Geospatial.GeoPoint bracketedPoint

multiPoint :: Trifecta.Parser Geospatial.GeoMultiPoint
multiPoint = do
  _ <- Trifecta.string "multipoint"
  _ <- Trifecta.spaces
  xl <- Wkt.emptySet <|> manyPoints
  pure $ Geospatial.GeoMultiPoint xl

manyPoints :: Trifecta.Parser (Vector.Vector Geospatial.GeoPositionWithoutCRS)
manyPoints = do
  _ <- Trifecta.char '(' >> Trifecta.spaces
  xl <- unbracketedPoints <|> bracketedPoints
  _ <- Trifecta.spaces >> Trifecta.char ')' >> Trifecta.spaces
  pure xl

unbracketedPoints :: Trifecta.Parser (Vector.Vector Geospatial.GeoPositionWithoutCRS)
unbracketedPoints = do
  x <- justPoints
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> justPoints)
  pure $ Vector.cons x (Vector.fromList xs)

bracketedPoints :: Trifecta.Parser (Vector.Vector Geospatial.GeoPositionWithoutCRS)
bracketedPoints = do
  x <- bracketedPoint
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> bracketedPoint)
  pure $ Vector.cons x (Vector.fromList xs)

bracketedPoint :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS
bracketedPoint = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  x <- justPoints
  _ <- Trifecta.spaces >> Trifecta.char ')' >> Trifecta.spaces
  pure x

justPoints :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS
justPoints = do
  x <- Wkt.number
  _ <- Trifecta.spaces
  y <- Wkt.number
  pure $ Geospatial.GeoPointXY (Geospatial.PointXY (Scientific.toRealFloat x) (Scientific.toRealFloat y))

emptyPoint :: Geospatial.GeoPoint
emptyPoint = Geospatial.GeoPoint Geospatial.GeoEmpty

emptyMultiPoint :: Geospatial.GeoMultiPoint
emptyMultiPoint = Geospatial.GeoMultiPoint Vector.empty
