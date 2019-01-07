module Data.Internal.Wkt.Point
  ( point
  , multiPoint
  , justPointsXY
  , justPointsXYZ
  , justPointsXYZM
  , emptyPoint
  , emptyMultiPoint
  ) where

import           Control.Applicative      ((<|>))
import qualified Data.Geospatial          as Geospatial
import qualified Data.Internal.Wkt.Common as Wkt
import qualified Data.Scientific          as Scientific
import qualified Data.Sequence            as Sequence
import qualified Text.Trifecta            as Trifecta

point :: Trifecta.Parser Geospatial.GeoPoint
point = do
  _ <- Trifecta.string "point"
  _ <- Trifecta.spaces
  (Trifecta.string "empty" >> pure emptyPoint) <|> nonEmptyPoints

nonEmptyPoints :: Trifecta.Parser Geospatial.GeoPoint
nonEmptyPoints =
  (Trifecta.string "zm" >> fmap Geospatial.GeoPoint (bracketedPoint justPointsXYZM))
  <|> (Trifecta.string "z" >> fmap Geospatial.GeoPoint (bracketedPoint justPointsXYZ))
  <|> fmap Geospatial.GeoPoint (bracketedPoint justPointsXY)

multiPoint :: Trifecta.Parser Geospatial.GeoMultiPoint
multiPoint = do
  _ <- Trifecta.string "multipoint"
  _ <- Trifecta.spaces
  xl <- Wkt.emptySet <|> nonEmptyMultipoints
  pure $ Geospatial.GeoMultiPoint xl

nonEmptyMultipoints :: Trifecta.Parser (Sequence.Seq Geospatial.GeoPositionWithoutCRS)
nonEmptyMultipoints =
  (Trifecta.string "zm" >> manyPoints justPointsXYZM)
  <|> (Trifecta.string "z" >> manyPoints justPointsXYZ)
  <|> manyPoints justPointsXY

manyPoints :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS -> Trifecta.Parser (Sequence.Seq Geospatial.GeoPositionWithoutCRS)
manyPoints pointParser = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  xl <- unbracketedPoints pointParser <|> bracketedPoints pointParser
  _ <- Trifecta.spaces >> Trifecta.char ')' >> Trifecta.spaces
  pure xl

unbracketedPoints :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS -> Trifecta.Parser (Sequence.Seq Geospatial.GeoPositionWithoutCRS)
unbracketedPoints pointParser = do
  x <- pointParser
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> pointParser )
  pure $ x Sequence.<| Sequence.fromList xs

bracketedPoints :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS -> Trifecta.Parser (Sequence.Seq Geospatial.GeoPositionWithoutCRS)
bracketedPoints pointParser = do
  x <- bracketedPoint pointParser
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> bracketedPoint pointParser)
  pure $ x Sequence.<| Sequence.fromList xs

bracketedPoint :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS -> Trifecta.Parser Geospatial.GeoPositionWithoutCRS
bracketedPoint pointParser = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  x <- pointParser
  _ <- Trifecta.spaces >> Trifecta.char ')' >> Trifecta.spaces
  pure x

firstXy :: Trifecta.Parser (Scientific.Scientific, Scientific.Scientific)
firstXy = do
  x <- Wkt.number
  _ <- Trifecta.spaces
  y <- Wkt.number
  pure (x, y)

firstXyz :: Trifecta.Parser (Scientific.Scientific, Scientific.Scientific, Scientific.Scientific)
firstXyz = do
  (x, y) <- firstXy
  _ <- Trifecta.spaces
  z <- Wkt.number
  pure (x, y, z)

justPointsXY :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS
justPointsXY = do
  (x,y) <- firstXy
  let xy = Geospatial.PointXY (Scientific.toRealFloat x) (Scientific.toRealFloat y)
  pure $ Geospatial.GeoPointXY xy

justPointsXYZ :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS
justPointsXYZ = do
  (x,y,z) <- firstXyz
  let xyz = Geospatial.PointXYZ (Scientific.toRealFloat x) (Scientific.toRealFloat y) (Scientific.toRealFloat z)
  pure $ Geospatial.GeoPointXYZ xyz

justPointsXYZM :: Trifecta.Parser Geospatial.GeoPositionWithoutCRS
justPointsXYZM = do
  (x,y,z) <- firstXyz
  _ <- Trifecta.spaces
  m <- Wkt.number
  let xyzm = Geospatial.PointXYZM (Scientific.toRealFloat x) (Scientific.toRealFloat y) (Scientific.toRealFloat z) (Scientific.toRealFloat m)
  pure $ Geospatial.GeoPointXYZM xyzm

emptyPoint :: Geospatial.GeoPoint
emptyPoint = Geospatial.GeoPoint Geospatial.GeoEmpty

emptyMultiPoint :: Geospatial.GeoMultiPoint
emptyMultiPoint = Geospatial.GeoMultiPoint Sequence.empty
