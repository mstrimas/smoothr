#' Densify lines or polygons
#'
#' This function adds additional vertices to lines or polygons via linear
#' interpolation, always while keeping the original vertices. Each line segment
#' will be split into equal length sub-segments. This densification algorithm
#' treats all vertices as Euclidean points, i.e. new points will not fall on a
#' great circle between existing vertices, rather they'll be along a straight
#' line.
#'
#' This function works on matrices of points and is generally not called
#' directly. Instead, use [smooth()] with `method = "densify"` to apply this
#' smoothing algorithm to spatial features.
#'
#' @param x numeric matrix; 2-column matrix of coordinates.
#' @param wrap logical; whether the coordinates should be wrapped at the ends,
#'   as for polygons and closed lines, to ensure a smooth edge.
#' @param n integer; number of times to split each line segment. Ignored if
#'   `max_distance` is specified.
#' @param max_distance numeric; the maximum distance between vertices in the
#'   resulting matrix. This is the Euclidean distance and not the great circle
#'   distance.
#'
#' @return A matrix with the coordinates of the densified curve.
#' @export
#' @examples
#' # smooth_densify works on matrices of coordinates
#' # use the matrix of coordinates defining a line as an example
#' m <- jagged_lines$geometry[[2]][]
#' m_dense <- smooth_densify(m, n = 5)
#' class(m)
#' class(m_dense)
#' plot(m, type = "b", pch = 19, cex = 1.5, axes = FALSE, xlab = NA, ylab = NA)
#' points(m_dense, col = "red", pch = 19, cex = 0.5)
#'
#' # max_distance can be used to ensure vertices are at most a given dist apart
#' m_md <- smooth_densify(m, max_distance = 0.05)
#' plot(m, type = "b", pch = 19, cex = 1.5, axes = FALSE, xlab = NA, ylab = NA)
#' points(m_md, col = "red", pch = 19, cex = 0.5)
#'
#' # smooth is a wrapper for smooth_densify that works on spatial features
#' library(sf)
#' l <- jagged_lines$geometry[[2]]
#' l_dense <- smooth(l, method = "densify", n = 2)
#' class(l)
#' class(l_dense)
#' plot(l, lwd = 5)
#' plot(l_dense, col = "red", lwd = 2, lty = 2, add = TRUE)
#' plot(l_dense %>% st_cast("MULTIPOINT"), col = "red", pch = 19,
#'      add = TRUE)
smooth_densify <- function(x, wrap = FALSE, n = 10L, max_distance) {
  stopifnot(is.matrix(x), ncol(x) == 2, nrow(x) > 1)
  n_pts <- nrow(x)
  # set densification parameters
  if (missing(max_distance)) {
    stopifnot(is_count(n), n >= 1)
    # n segments = n + 1 points
    # repeat for each segment
    n <- rep(n + 1, n_pts - 1)
  } else {
    stopifnot(is.numeric(max_distance), length(max_distance) == 1,
              max_distance > 0)
    # determine number of points based on max distance
    n <- ceiling(point_distance(x) / max_distance) + 1
  }
  # generate evenly spaced points for x and y
  x_dense <- seq_multiple(start = x[1:(n_pts - 1), 1],
                          end = x[2:n_pts, 1],
                          n = n)
  y_dense <- seq_multiple(start = x[1:(n_pts - 1), 2],
                          end = x[2:n_pts, 2],
                          n = n)
  # make sure start and end points are the same if wrapped
  if (wrap) {
    x_dense[length(x_dense)] <- x_dense[1]
    y_dense[length(y_dense)] <- y_dense[1]
  }
  cbind(x_dense, y_dense)
}
