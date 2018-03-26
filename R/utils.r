is_integer <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))
}

is_count <- function(x) {
  is_integer(x) && length(x) == 1 && x >= 0
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

point_distance <- function(x) {
  d <- diff(x)
  sqrt(d[, 1]^2 + d[, 2]^2)
}

seq_multiple <- function(start, end, n) {
  f <- function(x, y, z) {
    sq <- seq(from = x, to = y, length.out = z)
    # remove start since it will be duplicated by end of previous seqment
    sq[-1]
  }
  # interpolate evenly between a series of points
  sq_mult <- mapply(f, start, end, n, SIMPLIFY = FALSE)
  # combine seqments, need to add overall start point
  c(start[1], do.call(c, sq_mult))
}
