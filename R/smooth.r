#' Smooth a spatial feature
#'
#' @param x spatial features; lines or polygons from either the sf or sp
#'   packages.
#' @param method character; specifies the type of smoothing method to use.
#'   Possible methods are:
#'   - `"spline"`: uses spline interpolation via the [stats::spline()]
#'   function. This method interpolates between existing vertices and the
#'   resulting smoothed feature will pass through the vertices of the input
#'   feature.
#'
#' @return A smoothed polygon or line in the same format as the input data.
#' @references This function was inspired by the following StackExchange posts:
#' - [Create polygon from set of points distributed](https://stackoverflow.com/questions/26087772/#26089377)
#' - [Smoothing polygons in contour map?](https://gis.stackexchange.com/questions/24827/#24929)
#' @export
#' @examples
#' library(sf)
#' # spline smoothing
#' # example polygons
#' po <- par(mar = c(0, 0, 0, 0), oma = c(0, 0, 2, 0), mfrow = c(3, 3))
#' for (i in 1:nrow(jagged_polygons)) {
#'   p <- jagged_polygons[i, ]
#'   smoothed <- smooth(p, method = "spline")
#'   plot(st_geometry(smoothed), col = NA, border = NA)
#'   plot(st_geometry(p), col = "grey20", border = NA, add = TRUE)
#'   plot(st_geometry(smoothed), col = NA, border = "red", lwd = 2, add = TRUE)
#'   title("Smoothed Polygons (Spline Interpolation)", cex.main = 2, outer = TRUE)
#' }
#' par(po)
#'
#' # example lines
#' po <- par(mar = c(0, 0, 0, 0), oma = c(0, 0, 2, 0), mfrow = c(3, 3))
#' for (i in 1:nrow(jagged_lines)) {
#'   l <- jagged_lines[i, ]
#'   smoothed <- smooth(l, method = "spline")
#'   plot(st_geometry(smoothed), col = NA)
#'   plot(st_geometry(l), col = "grey20", lwd = 2, add = TRUE)
#'   plot(st_geometry(smoothed), col = "red", lwd = 2, add = TRUE)
#'   title("Smoothed Lines (Spline Interpolation)", cex.main = 2, outer = TRUE)
#' }
#' par(po)
smooth <- function(x, method = c("spline")) {
  UseMethod("smooth")
}

#' @export
smooth.sfg <- function(x, method = c("spline")) {
  method <- match.arg(method)
  if (sf::st_is(x, "LINESTRING")) {
    x <- sf::st_linestring(smooth_spline(x[], type = "line"))
  } else if (sf::st_is(x, "POLYGON")) {
    for (i in seq_along(x)) {
      x[[i]] <- smooth_spline(x[[i]], type = "polygon")
    }
  } else if (sf::st_is(x, "MULTILINESTRING")) {
    for (i in seq_along(x)) {
      x[[i]] <- smooth_spline(x[[i]], type = "line")
    }
  } else if (sf::st_is(x, "MULTIPOLYGON")) {
    for (i in seq_along(x)) {
      for (j in seq_along(x[[i]])) {
        x[[i]][[j]] <- smooth_spline(x[[i]][[j]], type = "polygon")
      }
    }
  } else {
    stop("smooth only valid for line and polygon features")
  }
  x
}

#' @export
smooth.sfc <- function(x, method = c("spline")) {
  method <- match.arg(method)
  for (i in seq_along(x)) {
    x[[i]] <- smooth(x[[i]], method = method)
  }
  sf::st_sfc(x)
}

#' @export
smooth.sf <- function(x, method = c("spline")) {
  method <- match.arg(method)
  sf::st_geometry(x) <- smooth(sf::st_geometry(x), method = method)
  x
}

#' @export
smooth.Spatial <- function(x, method = c("spline")) {
  if (!requireNamespace("sp", quietly = TRUE)) {
    stop("Install the sp package to smooth sp features.")
  }
  method <- match.arg(method)
  # convert to sf object then back
  if (inherits(x, c("SpatialPolygons", "SpatialLines"))) {
    smoothed <- smooth(sf::st_as_sfc(x), method = method)
  } else if (inherits(x, c("SpatialPolygonsDataFrame", "SpatialLinesDataFrame"))) {
    smoothed <- smooth(sf::st_as_sf(x), method = method)
  } else {
    stop(paste("No smooth method for class", class(x)))
  }
  methods::as(smoothed, "Spatial")
}

smooth_spline <- function(x, type = c("line", "polygon")) {
  stopifnot(is.matrix(x), ncol(x) == 2)
  type <- match.arg(type)
  if (type == "line") {
    if (all(x[1, ] == x[nrow(x), ])) {
      method <- "periodic"
    } else {
      method <- "fmm"
    }
  } else {
    method <- "periodic"
  }
  x1 <- stats::spline(1:nrow(x), x[, 1], n = 1000, method = method)$y
  x2 <- stats::spline(1:nrow(x), x[, 2], n = 1000, method = method)$y
  cbind(x1, x2)
}
