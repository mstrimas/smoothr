#' Chaikin's corner cutting algorithm
#'
#' Chaikin's corner cutting algorithm smooths a curve by iteratively replacing
#' every point by two new points: one 1/4 of the way to the next point and one
#' 1/4 of the way to the previous point.
#'
#' This function works on matrices of points and is generally not called
#' directly. Instead, use [smooth()] with `method = "chaikin"` to apply this
#' smoothing algorithm to spatial features.
#'
#' @param x numeric matrix; 2-column matrix of coordinates.
#' @param wrap logical; whether the coordinates should be wrapped at the ends,
#'   as for polygons and closed lines, to ensure a smooth edge.
#' @param refinements integer; number of corner cutting iterations to apply.
#'
#' @return A matrix with the coordinates of the smoothed curve.
#' @references The original reference for Chaikin's corner cutting algorithm is:
#'
#'   - `Chaikin, G. An algorithm for high speed curve generation. Computer Graphics and Image Processing 3 (1974), 346–349`
#'
#'   This implementation was inspired by the following StackOverflow answer:
#'
#'   - [Where to find Python implementation of Chaikin's corner cutting algorithm?](https://stackoverflow.com/a/47255374/3591386)
#' @seealso [smooth()]
#' @export
#' @examples
#' # smooth_chaikin works on matrices of coordinates
#' # use the matrix of coordinates defining a polygon as an example
#' m <- jagged_polygons$geometry[[2]][[1]]
#' m_smooth <- smooth_chaikin(m, wrap = TRUE)
#' class(m)
#' class(m_smooth)
#' plot(m, type = "l", axes = FALSE, xlab = NA, ylab = NA)
#' lines(m_smooth, col = "red")
#'
#' # smooth is a wrapper for smooth_chaikin that works on spatial features
#' library(sf)
#' p <- jagged_polygons$geometry[[2]]
#' p_smooth <- smooth(p, method = "chaikin")
#' class(p)
#' class(p_smooth)
#' plot(p)
#' plot(p_smooth, border = "red", add = TRUE)
smooth_chaikin <- function(x, wrap = FALSE, refinements = 3L) {
  stopifnot(is.matrix(x), ncol(x) == 2)
  stopifnot(is_flag(wrap))
  stopifnot(is_count(refinements), refinements <= 10)

  # polygons and closed lines need to be wrapped
  if (wrap) {
    for (i in seq.int(refinements)) {
      n_pts <- nrow(x)
      qr <- matrix(NA_real_, nrow = 2 * (n_pts - 1) + 1, ncol = 2)
      qr[seq(1, nrow(qr) - 1, by = 2), ] <- 0.75 * x[-n_pts, ] + 0.25 * x[-1, ]
      qr[seq(2, nrow(qr) - 1, by = 2), ] <- 0.75 * x[-1, ] + 0.25 * x[-n_pts, ]
      qr[nrow(qr), ] <- qr[1, ]
      x <- qr
    }
  # lines should have endpoints fixed
  } else {
    for (i in seq.int(refinements)) {
      n_pts <- nrow(x)
      qr <- matrix(NA_real_, nrow = 2 * (n_pts - 1), ncol = 2)
      qr[seq(1, nrow(qr), by = 2), ] <- 0.75 * x[-n_pts, ] + 0.25 * x[-1, ]
      qr[seq(2, nrow(qr), by = 2), ] <- 0.75 * x[-1, ] + 0.25 * x[-n_pts, ]
      qr[1, ] <- x[1, ]
      qr[nrow(qr), ] <- x[nrow(x), ]
      x <- qr
    }
  }
  x
}
