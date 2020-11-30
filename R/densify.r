#' Densify spatial lines or polygons
#'
#' A wrapper for `smooth(x, method = "densify")`. This function adds additional
#' vertices to spatial feature via linear interpolation, always while keeping
#' the original vertices. Each line segment will be split into equal length
#' sub-segments. This densification algorithm treats all vertices as Euclidean
#' points, i.e. new points will not fall on a great circle between existing
#' vertices, rather they'll be along a straight line.
#'
#' @param x spatial features; lines or polygons from either the `sf`, `sp`, or
#'   `terra` packages.
#' @inheritParams smooth_densify
#'
#' @return A densified polygon or line in the same format as the input data.
#' @export
#' @examples
#' library(sf)
#' l <- jagged_lines$geometry[[2]]
#' l_dense <- densify(l, n = 2)
#' plot(l, lwd = 5)
#' plot(l_dense, col = "red", lwd = 2, lty = 2, add = TRUE)
#' plot(l_dense %>% st_cast("MULTIPOINT"), col = "red", pch = 19,
#'      add = TRUE)
densify <- function(x, n = 10L, max_distance) {
  if (missing(max_distance)) {
    smooth(x, method = "densify", n = n)
  } else {
    smooth(x, method = "densify", max_distance = max_distance)
  }
}
