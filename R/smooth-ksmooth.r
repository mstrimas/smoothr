#' Kernel smooth
#'
#' Kernel smoothing uses [stats::ksmooth()] to smooth out existing vertices
#' using Gaussian kernel regression. Kernel smoothing is applied to the `x` and
#' `y` coordinates are independently. Prior to smoothing, [smooth_densify()] is
#' called to generate additional vertices, and the smoothing is applied to this
#' densified set of vertices.
#'
#' This function works on matrices of points and is generally not called
#' directly. Instead, use [smooth()] with `method = "ksmooth"` to apply this
#' smoothing algorithm to spatial features.
#'
#' @param x numeric matrix; 2-column matrix of coordinates.
#' @param wrap logical; whether the coordinates should be wrapped at the ends,
#'   as for polygons and closed lines, to ensure a smooth edge.
#' @param n integer; number of times to split each line segment for
#'   [smooth_densify()]. Ignored if `max_distance` is specified.
#' @param max_distance numeric; the maximum distance between vertices for
#'   [smooth_densify()]. This is the Euclidean distance and not the great circle
#'   distance.
#' @param bandwidth numeric; the bandwidth of the Gaussian kernel. Larger
#'   bandwidths will result in more smoothing. If no value is supplied, the
#'   bandwidth is estimated from the data.
#'
#' @return A matrix with the coordinates of the smoothed curve.
#' @references The kernel smoothing method was inspired by the following
#'   StackExchange answers:
#'
#'   - [Nadaraya-Watson Optimal Bandwidth](https://stats.stackexchange.com/a/143608/44268)
#'   - [Smoothing polygons in contour map?](https://gis.stackexchange.com/a/24929/26661)
#' @seealso [smooth()]
#' @export
#' @examples
#' # smooth_ksmooth works on matrices of coordinates
#' # use the matrix of coordinates defining a polygon as an example
#' m <- jagged_polygons$geometry[[2]][[1]]
#' m_smooth <- smooth_ksmooth(m, wrap = TRUE)
#' class(m)
#' class(m_smooth)
#' plot(m, type = "l", col = "black", lwd = 3, axes = FALSE, xlab = NA,
#'      ylab = NA)
#' lines(m_smooth, lwd = 3, col = "red")
#'
#' # lines can also be smoothed
#' l <- jagged_lines$geometry[[2]][]
#' l_smooth <- smooth_ksmooth(l, wrap = FALSE, max_distance = 0.05, bandwidth = 10)
#' plot(l, type = "l", col = "black", lwd = 3, axes = FALSE, xlab = NA,
#'      ylab = NA)
#' lines(l_smooth, lwd = 3, col = "red")
#'
#' # smooth is a wrapper for smooth_ksmooth that works on spatial features
#' library(sf)
#' p <- jagged_polygons$geometry[[2]]
#' p_smooth <- smooth(p, method = "ksmooth")
#' class(p)
#' class(p_smooth)
#' plot(p_smooth, border = "red")
#' plot(p, add = TRUE)
smooth_ksmooth <- function(x, wrap = FALSE, n = 10L, max_distance, bandwidth) {
  stopifnot(is.matrix(x), ncol(x) == 2, nrow(x) > 1)
  stopifnot(is_flag(wrap))

  # first densify
  x <- smooth_densify(x, wrap = wrap, n = n, max_distance = max_distance)
  n_pts <- nrow(x)

  # choose bandwith
  if (missing(bandwidth)) {
    bandwidth <- sqrt((n_pts^2 - 1) / 12) * n_pts^(-1/5)
  }
  stopifnot(is.numeric(bandwidth), length(bandwidth) == 1, bandwidth > 0)

  # wrap vertices as necessary
  if (wrap) {
    x <- rbind(x[-n_pts, ], x, x[-1, ])
    n_wrapped <- nrow(x)
    x_smooth <- stats::ksmooth(1:n_wrapped, x[, 1], n.points = n_wrapped,
                               kernel = "normal", bandwidth = bandwidth)$y
    y_smooth <- stats::ksmooth(1:n_wrapped, x[, 2], n.points = n_wrapped,
                               kernel = "normal", bandwidth = bandwidth)$y
    # remove extra wrapped vertices
    x <- cbind(x_smooth, y_smooth)[n_pts:(2 * n_pts - 1), ]
    # ensure the endpoints match
    x[nrow(x), ] <- x[1, ]
  } else {
    x_smooth <- stats::ksmooth(1:n_pts, x[, 1], n.points = n_pts,
                               kernel = "normal", bandwidth = bandwidth)$y
    y_smooth <- stats::ksmooth(1:n_pts, x[, 2], n.points = n_pts,
                               kernel = "normal", bandwidth = bandwidth)$y
    # remove extra wrapped vertices
    x <- cbind(x_smooth, y_smooth)
  }
  return(x)
}
