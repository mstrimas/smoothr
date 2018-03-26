#' Spline interpolation
#'
#' Spline interpolation uses [stats::spline()] to interpolate between existing
#' vertices using piecewise cubic polynomials. The `x` and `y` coordinates are
#' interpolated independently. The curve will always pass through the vertices
#' of the original feature.
#'
#' This function works on matrices of points and is generally not called
#' directly. Instead, use [smooth()] with `method = "spline"` to apply this
#' smoothing algorithm to spatial features.
#'
#' @param x numeric matrix; 2-column matrix of coordinates.
#' @param wrap logical; whether the coordinates should be wrapped at the ends,
#'   as for polygons and closed lines, to ensure a smooth edge.
#' @param vertex_factor double; the proportional increase in the number of
#'   vertices in the smooth curve. For example, if the original curve has 100
#'   points, a value of `2.5` will yield a new smoothed curve with 250 points.
#'   Ignored if `n` is specified.
#' @param n integer; number of vertices in the smoothed curve.
#'
#' @return A matrix with the coordinates of the smoothed curve.
#' @references The spline method was inspired by the following StackExchange
#'   answers:
#'
#'   - [Create polygon from set of points distributed](https://stackoverflow.com/a/26089377/3591386)
#'   - [Smoothing polygons in contour map?](https://gis.stackexchange.com/a/24929/26661)
#' @seealso [smooth()]
#' @export
#' @examples
#' # smooth_spline works on matrices of coordinates
#' # use the matrix of coordinates defining a polygon as an example
#' m <- jagged_polygons$geometry[[2]][[1]]
#' m_smooth <- smooth_spline(m, wrap = TRUE)
#' class(m)
#' class(m_smooth)
#' plot(m_smooth, type = "l", col = "red", axes = FALSE, xlab = NA, ylab = NA)
#' lines(m, col = "black")
#'
#' # smooth is a wrapper for smooth_spline that works on spatial features
#' library(sf)
#' p <- jagged_polygons$geometry[[2]]
#' p_smooth <- smooth(p, method = "spline")
#' class(p)
#' class(p_smooth)
#' plot(p_smooth, border = "red")
#' plot(p, add = TRUE)
smooth_spline <- function(x, wrap = FALSE, vertex_factor = 5, n) {
  stopifnot(is.matrix(x), ncol(x) == 2, nrow(x) > 1)
  n_pts <- nrow(x)
  stopifnot(is_flag(wrap))
  if (missing(n)) {
    stopifnot(is.double(vertex_factor), length(vertex_factor) == 1,
              vertex_factor >= 1)
    n <- max(round(vertex_factor * n_pts), n_pts)
  } else {
    stopifnot(is_count(n), n >= n_pts)
  }
  if (wrap) {
      method <- "periodic"
  } else {
    method <- "fmm"
  }
  x1 <- stats::spline(seq_len(n_pts), x[, 1], n = n, method = method)$y
  x2 <- stats::spline(seq_len(n_pts), x[, 2], n = n, method = method)$y
  cbind(x1, x2)
}
