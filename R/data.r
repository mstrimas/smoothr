#' Jagged polygons for smoothing
#'
#' Spatial polygons in `sf` format for smoothing. Most of these polygons have
#' been created by converting rasters to polygons and therefore consist entirely
#' of right angles. There are examples of polygons with holes and multipart
#' polygons.
#'
#' @format An `sf` object with 9 features and 3 attribute:
#'   - **`type`**: character; the geometry, i.e. "polygon" or "line".
#'   - **`hole`**: logical; whether the polygon has holes or not.
#'   - **`multipart`**: logical; whether the feature is single or multipart.
"jagged_polygons"

#' Jagged lines for smoothing
#'
#' Spatial lines in `sf` format for smoothing. There are examples of lines
#' forming a closed loop and multipart lines.
#'
#' @format An `sf` object with 9 features and 3 attribute:
#'   - **`type`**: character; the geometry, i.e. "polygon" or "line".
#'   - **`closed`**: logical; whether the line forms a closed loop or not.
#'   - **`multipart`**: logical; whether the feature is single or multipart.
"jagged_lines"
