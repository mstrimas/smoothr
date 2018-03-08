smooth_spline <- function(x, type = c("polygon", "line"), n_vertices = 1000) {
  stopifnot(is.matrix(x), ncol(x) == 2)
  type <- match.arg(type)
  if (type == "polygon" || all(x[1, ] == x[nrow(x), ])) {
      method <- "periodic"
  } else {
    method <- "fmm"
  }
  x1 <- stats::spline(1:nrow(x), x[, 1], n = n_vertices, method = method)$y
  x2 <- stats::spline(1:nrow(x), x[, 2], n = n_vertices, method = method)$y
  cbind(x1, x2)
}
