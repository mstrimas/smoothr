#' Jagged polygons for smoothing
#'
#' Spatial polygons in [sf][sf::sf] format for smoothing. Most of these polygons
#' have been created by converting rasters to polygons and therefore consist
#' entirely of right angles. There are examples of polygons with holes and
#' multipart polygons.
#'
#' @format An [sf][sf::sf]  object with 9 features and 3 attribute:
#'   - **`type`**: character; the geometry, i.e. "polygon" or "line".
#'   - **`hole`**: logical; whether the polygon has holes or not.
#'   - **`multipart`**: logical; whether the feature is single or multipart.
"jagged_polygons"

#' Jagged lines for smoothing
#'
#' Spatial lines in [sf][sf::sf] format for smoothing. There are examples of
#' lines forming a closed loop and multipart lines.
#'
#' @format An [sf][sf::sf]  object with 9 features and 3 attribute:
#'   - **`type`**: character; the geometry, i.e. "polygon" or "line".
#'   - **`closed`**: logical; whether the line forms a closed loop or not.
#'   - **`multipart`**: logical; whether the feature is single or multipart.
"jagged_lines"

#' 3D jagged line with Z-dimension for smoothing
#'
#' Spatial lines in [sf][sf::sf]  format for smoothing in three dimensions.
#' There are examples of open and closed loops
#'
#' @format An [sf][sf::sf] object with 9 features and 3 attribute:
#'   - **`type`**: character; the geometry, i.e. "polygon" or "line".
#'   - **`closed`**: logical; whether the line forms a closed loop or not.
#'   - **`multipart`**: logical; whether the feature is single or multipart.
"jagged_lines_3d"

#' Simulated raster for polygonizing and smoothing
#'
#' One of the primary applications of this package is for smoothing polygons
#' generated from rasters. This example raster dataset is meant to be a
#' simulated occurrence probability for a species, consisting of a spatially
#' auto-correlated Gaussian field with values between 0 and 1. This raster is a
#' 25x25 grid of 100 square kilometer cells in a North American centered Albers
#' Equal Area projection.
#'
#' @format A wrapped [SpatRaster][terra::rast()] object with one layer. Call
#'   [terra::rast()] to unwrap it.
"jagged_raster"
