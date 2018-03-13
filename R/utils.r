is_integer <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))
}

is_count <- function(x) {
  is_integer(x) && length(x) == 1 && x >= 0
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}
