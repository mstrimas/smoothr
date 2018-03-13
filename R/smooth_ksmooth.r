#' Kernel smoothing
#'
#' Kernel smoothing uses [stats::ksmooth()] to estimate a curve as a weighted
#' average of the existing vertices, where the weights are defined by an
#' Guassian kernel. The `x` and `y` coordinates are smoothed independently.
#'
#' This function works on matrices of points and is generally not called
#' directly. Instead, use [smooth()] with `method = "ksmooth"` to apply this
#' smoothing algorithm to spatial features.
#'
#' @param x numeric matrix; 2-column matrix of coordinates.
#' @param wrap logical; whether the coordinates should be wrapped at the ends,
#'   as for polygons and closed lines.
#' @param bandwidth double; the kernel bandwidth, larger values lead to more
#'   smoothing. Leaving `bandwidth` blank will estimate a reasonable bandwith
#'   from the data. Typically it's best to start with this default value, then
#'   refine as necessary to get the desired amount of smoothing.
#' @param n integer; number of vertices in the smoothed curve. Ignored if
#'   `vertex_factor` is specified.
#' @param vertex_factor double; the proportional increase in the number of
#'   vertices in the smooth curve. For example, if the oringal curve has 10
#'   points, a value of `2.5` will yield a new smoothed curve with 250 points.
#' @param k integer; number of vertices to wrap around at each end to ensure
#'   that the boundary of a polygon is correctly smoothed at the endpoints.
#'
#' @return A matrix with the coordiantes of the smoothed curve.
#' @references The formula for estimating a default bandwith is taken from the
#'   following StackExchange answer:
#'
#'   - [Nadaraya-Watson Optimal Bandwidth](https://stats.stackexchange.com/a/143608/44268)
#'
#' @seealso [smooth()]
#' @export
#' @examples
#' # smooth_ksmooth() works on matrices of coordinates
#' # use the matrix of coordinates defining a polygon as an example
#' m <- jagged_polygons$geometry[[1]][[1]]
#' m_smooth <- smooth_ksmooth(m, n = 100, bandwidth = 0.25)
#' class(m)
#' class(m_smooth)
#' plot(m, type = "l", col = "black", axes = FALSE, xlab = NA, ylab = NA)
#' lines(m_smooth, col = "red")
#' points(m_smooth)
#'
#' # smooth() is a wrapper for smooth_ksmooth that works on spatial features
#' library(sf)
#' p <- jagged_polygons$geometry[[2]]
#' p_smooth <- smooth(p, method = "ksmooth")
#' class(p)
#' class(p_smooth)
#' plot(p_smooth, border = "red")
#' plot(p, add = TRUE)
smooth_ksmooth <- function(x, wrap = TRUE, bandwidth, n = 100, vertex_factor,
                           k = 3) {
  stopifnot(is.matrix(x), ncol(x) == 2)
  stopifnot(is_flag(wrap))
  n_pts <- nrow(x)
  if (missing(bandwidth)) {
    bandwidth <- sqrt((n_pts^2 - 1) / 12) * n_pts^(-1/5)
    message(paste0("Smoothing with kernel bandwith: ",
                   format(bandwidth, digitgs = 3)))
  } else {
    stopifnot(is.double(bandwidth), length(bandwidth) == 1, bandwidth > 0)
  }
  stopifnot(is_count(n), n >= n_pts)
  if (!missing(vertex_factor)) {
    stopifnot(is.double(vertex_factor), length(vertex_factor) == 1,
              vertex_factor >= 1)
    n <- max(round(vertex_factor * nrow(x)), nrow(x))
  }
  if (wrap) {
    # wrap k vertices around ends
    if (k >= 1) {
      x <- rbind(x[(n_pts - k):(n_pts - 1), ], x, x[2:(k + 1), ])
    }
  } else {
    k = 0
  }
  sx <- stats::ksmooth(1:nrow(x), x[, 1], kernel = "normal",
                       bandwidth = bandwidth, n.points = n)
  sy <- stats::ksmooth(1:nrow(x), x[, 2], kernel = "normal",
                       bandwidth = bandwidth, n.points = n)
  cbind(sx$y, sy$y)[k < sx$x & sx$x <= n + k, ]
}
