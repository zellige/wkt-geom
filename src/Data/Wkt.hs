module Data.Wkt
  ( parseByteString
  , parseString
  , WktGeometryCollection.geometryCollection
  , WktGeometryCollection.emptyGeometryCollection
  , WktLine.emptyLine
  , WktLine.emptyMultiLine
  , WktLine.lineString
  , WktLine.multiLineString
  , WktPoint.emptyPoint
  , WktPoint.emptyMultiPoint
  , WktPoint.point
  , WktPoint.multiPoint
  , WktPolygon.emptyPolygon
  , WktPolygon.emptyMultiPolygon
  , WktPolygon.polygon
  , WktPolygon.multiPolygon
  ) where

import qualified Data.ByteString                      as ByteString
import qualified Data.ByteString.UTF8                 as UTF8
import qualified Data.Char                            as Char
import qualified Text.Trifecta                        as Trifecta
import qualified Text.Trifecta.Delta                  as TrifectaDelta

import qualified Data.Internal.Wkt.GeometryCollection as WktGeometryCollection
import qualified Data.Internal.Wkt.Line               as WktLine
import qualified Data.Internal.Wkt.Point              as WktPoint
import qualified Data.Internal.Wkt.Polygon            as WktPolygon

delta :: String -> TrifectaDelta.Delta
delta str = TrifectaDelta.Directed (UTF8.fromString str) 0 0 0 0

parseByteString :: Trifecta.Parser a -> ByteString.ByteString -> Trifecta.Result a
parseByteString p bs = Trifecta.parseByteString p (TrifectaDelta.Directed lowerBs 0 0 0 0) lowerBs
  where
    lowerBs = d8ToLower bs
    d8ToLower = ByteString.map f
      where
        f w | w >= 65 && w <= 90 = w + 32
            | otherwise = w

parseString :: Trifecta.Parser a -> String -> Trifecta.Result a
parseString p s = Trifecta.parseString p (Data.Wkt.delta lowerS) lowerS
  where
    lowerS = asciiToLower s
    asciiToLower = fmap f
      where
        f c | Char.isAsciiUpper c = Char.chr (Char.ord c + 32)
            | otherwise = c
