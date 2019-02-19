# Changelog
All notable changes to this project will be documented in this file.

## [0.0.6] - 2019-02-11
### Fixed
- EWKB toByteString was missing SRID [https://github.com/indicatrix/wkt-geom/pull/9].

## [0.0.5] - 2019-01-07
### Added
- Add serializers back into ByteString [https://github.com/indicatrix/wkt-geom/issues/7].
- New API for WKB which takes hex (base 16) encoded ByteString [https://github.com/indicatrix/wkt-geom/issues/6].
- Improvements in testing (added Hedgehog).
- Support for WKT encodings for 3D (XYZ) and 4D (XYZM) points, lines and polygons.