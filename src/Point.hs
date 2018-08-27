module Point (
  emptyPoint
, emptyMultiPoint
, justPoints
, point
, multiPoint
) where

import           Control.Applicative    ((<|>))
import qualified Data.Geography.GeoJSON as GeoJSON
import qualified Data.Scientific        as Scientific
import qualified Text.Trifecta          as Trifecta

import qualified Wkt

point :: Trifecta.Parser GeoJSON.PointGeometry
point = do
  _ <- Trifecta.string "point"
  _ <- Trifecta.spaces
  x <- (Trifecta.string "empty" >> pure emptyPoint) <|> bracketedPoint
  pure x

multiPoint :: Trifecta.Parser GeoJSON.MultiPointGeometry
multiPoint = do
  _ <- Trifecta.string "multipoint"
  _ <- Trifecta.spaces
  xl <- Wkt.emptySet <|> manyPoints
  pure (GeoJSON.MultiPointGeometry xl)

manyPoints :: Trifecta.Parser [GeoJSON.PointGeometry]
manyPoints = do
  _ <- Trifecta.char '(' >> Trifecta.spaces
  xl <- unbracketedPoints <|> bracketedPoints
  _ <- Trifecta.spaces >> Trifecta.char ')' >> Trifecta.spaces
  pure xl

unbracketedPoints :: Trifecta.Parser [GeoJSON.PointGeometry]
unbracketedPoints = do
  x <- pointText
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> pointText)
  pure (x:xs)

bracketedPoints :: Trifecta.Parser [GeoJSON.PointGeometry]
bracketedPoints = do
  x <- bracketedPoint
  xs <- Trifecta.many (Trifecta.char ',' >> Trifecta.spaces >> bracketedPoint)
  pure (x:xs)

bracketedPoint :: Trifecta.Parser GeoJSON.PointGeometry
bracketedPoint = do
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  x <- pointText
  _ <- Trifecta.spaces >> Trifecta.char ')' >> Trifecta.spaces
  pure x

pointText :: Trifecta.Parser GeoJSON.PointGeometry
pointText = GeoJSON.PointGeometry <$> justPoints

justPoints :: Trifecta.Parser [Scientific.Scientific]
justPoints = do
  x <- Wkt.number
  _ <- Trifecta.spaces
  y <- Wkt.number
  pure [x, y]

emptyPoint :: GeoJSON.PointGeometry
emptyPoint = GeoJSON.PointGeometry []

emptyMultiPoint :: GeoJSON.MultiPointGeometry
emptyMultiPoint = GeoJSON.MultiPointGeometry []
