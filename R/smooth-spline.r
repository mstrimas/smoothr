#' Spline interpolation
#'
#' Spline interpolation uses [stats::spline()] to interpolate between existing
#' vertices using piecewise cubic polynomials. The `x` and `y` coordinates are
#' interpolated independently, and .
#'
#' This function works on matrices of points and is generally not called
#' directly. Instead, use [smooth()] with `method = "spline"` to apply this
#' smoothing algorithm to spatial features.
#'
#' @param x numeric matrix; 2-column matrix of coordinates.
#' @param type character; whether the coordinates correspond to a line or
#'   polygon.
#' @param n integer; number of vertices in the smoothed curve. Ignored if
#'   `smoothness` is specified.
#' @param smoothness double; the proportional increase in the number of vertices
#'   in the smooth curve. For example, if the oringal curve has 10 points, a
#'   value of `2.5` will yield a new smoothed curve with 250 points.
#'
#' @return A matrix with the coordiantes of the smoothed curve.
#' @references The spline method was inspired by the following StackExchange
#'   posts:
#'
#'   - [Create polygon from set of points distributed](https://stackoverflow.com/questions/26087772/26089377)
#'   - [Smoothing polygons in contour map?](https://gis.stackexchange.com/questions/24827/24929)
#' @seealso [smooth()]
#' @export
#' @examples
#' library(sf)
#' # smooth polygons
#' p_smoothed <- smooth(jagged_polygons, method = "spline")
#' par(mar = c(0, 0, 0, 0), oma = c(0, 0, 2, 0), mfrow = c(3, 3))
#' for (i in 1:nrow(jagged_polygons)) {
#'   plot(st_geometry(p_smoothed[i, ]), col = NA, border = NA)
#'   plot(st_geometry(jagged_polygons[i, ]), col = "grey20", border = NA, add = TRUE)
#'   plot(st_geometry(p_smoothed[i, ]), col = NA, border = "red", lwd = 2, add = TRUE)
#'   title("Smoothed Polygons (Spline Interpolation)", cex.main = 2, outer = TRUE)
#' }
#'
#' # smooth lines
#' l_smoothed <- smooth(jagged_lines, method = "spline")
#' par(mar = c(0, 0, 0, 0), oma = c(0, 0, 2, 0), mfrow = c(3, 3))
#' for (i in 1:nrow(jagged_lines)) {
#'   plot(st_geometry(l_smoothed[i, ]), col = NA)
#'   plot(st_geometry(jagged_lines[i, ]), col = "grey20", lwd = 2, add = TRUE)
#'   plot(st_geometry(l_smoothed[i, ]), col = "red", lwd = 2, add = TRUE)
#'   title("Smoothed Lines (Spline Interpolation)", cex.main = 2, outer = TRUE)
#' }
smooth_spline <- function(x, type = c("polygon", "line"), n = 100,
                          smoothness = NULL) {
  stopifnot(is.matrix(x), ncol(x) == 2)
  stopifnot(is_count(n))
  type <- match.arg(type)
  if (!is.null(smoothness)) {
    stopifnot(is.double(smoothness), length(smoothness) != 1, smoothness >= 1)
    n <- max(round(smoothness * n), n)
  }
  if (type == "polygon" || all(x[1, ] == x[nrow(x), ])) {
      method <- "periodic"
  } else {
    method <- "fmm"
  }
  x1 <- stats::spline(1:nrow(x), x[, 1], n = n, method = method)$y
  x2 <- stats::spline(1:nrow(x), x[, 2], n = n, method = method)$y
  cbind(x1, x2)
}
