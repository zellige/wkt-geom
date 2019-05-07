module Data.Internal.Wkt.Box
  ( box ) where

import qualified Data.Geospatial         as Geospatial
import qualified Text.Trifecta           as Trifecta

import qualified Data.Internal.Wkt.Line  as Line
import qualified Data.Internal.Wkt.Point as Point

box :: Trifecta.Parser Geospatial.BoundingBoxWithoutCRS
box = do
  _ <- Trifecta.string "box"
  _ <- Trifecta.spaces
  _ <- Trifecta.spaces >> Trifecta.char '(' >> Trifecta.spaces
  (Geospatial.GeoPointXY first) <- Point.justPointsXY
  (Geospatial.GeoPointXY second) <- Line.commandPoint Point.justPointsXY
  _ <- Trifecta.char ')' >> Trifecta.spaces
  pure $ Geospatial.BoundingBoxWithoutCRSXY first second
