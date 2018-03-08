smooth_chaikin <- function(x, type = c("polygon", "line"), refinements = 5) {
  stopifnot(is.matrix(x), ncol(x) == 2)
  type <- match.arg(type)
  if (type == "polygon" || all(x[1, ] == x[nrow(x), ])) {
    for (i in seq.int(refinements)) {
      n_pts <- nrow(x)
      qr <- matrix(NA_real_, nrow = 2 * (n_pts - 1) + 1, ncol = 2)
      qr[seq(1, nrow(qr) - 1, by = 2), ] <- 0.75 * x[-n_pts, ] + 0.25 * x[-1, ]
      qr[seq(2, nrow(qr) - 1, by = 2), ] <- 0.75 * x[-1, ] + 0.25 * x[-n_pts, ]
      qr[nrow(qr), ] <- qr[1, ]
      x <- qr
    }
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
