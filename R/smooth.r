#' Smooth a spatial feature
#'
#' @param x spatial features; lines or polygons from either the sf or sp
#'   packages.
#' @param method character; specifies the type of smoothing method to use.
#'   Possible methods are:
#'
#'   - `"spline"`: spline interpolation via the [stats::spline()] function. This
#'   method interpolates between existing vertices and the resulting smoothed
#'   feature will pass through the vertices of the input feature.
#'   - `"chaikin"`: Chaikin's corner cutting algorithm, which smooths a line by
#'   iteratively replacing every point by two new points: one 25% of the way to
#'   the next point and one 25% of the way to the previous point.
#'
#' @return A smoothed polygon or line in the same format as the input data.
#' @references A variety of resources were used in the implementation of each of
#'   the methods in this function. The spline method was inspired by the
#'   following StackExchange posts:
#'
#'   - [Create polygon from set of points distributed](https://stackoverflow.com/questions/26087772/26089377)
#'   - [Smoothing polygons in contour map?](https://gis.stackexchange.com/questions/24827/24929)
#'
#'   Chaikin's corner curring algorithm was based on:
#'
#'   - `Chaikin, G. An algorithm for high speed curve generation. Computer Graphics and Image Processing 3 (1974), 346–349`
#'   - [Where to find Python implementation of Chaikin's corner cutting algorithm?](https://stackoverflow.com/a/47255374/3591386)
#' @export
#' @examples
#' library(sf)
#' # spline interpolation
#' # polygons
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
#' # lines
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
#'
#' # chaikin's corner cutting
#' # polygons
#' po <- par(mar = c(0, 0, 0, 0), oma = c(0, 0, 2, 0), mfrow = c(3, 3))
#' for (i in 1:nrow(jagged_polygons)) {
#'   p <- jagged_polygons[i, ]
#'   smoothed <- smooth(p, method = "chaikin")
#'   plot(st_geometry(p), col = "grey20", border = NA)
#'   plot(st_geometry(smoothed), col = NA, border = "red", lwd = 2, add = TRUE)
#'   title("Smoothed Polygons (Chaikin's Corner Cutting)", cex.main = 2, outer = TRUE)
#' }
#' par(po)
#' # lines
#' po <- par(mar = c(0, 0, 0, 0), oma = c(0, 0, 2, 0), mfrow = c(3, 3))
#' for (i in 1:nrow(jagged_lines)) {
#'   l <- jagged_lines[i, ]
#'   smoothed <- smooth(l, method = "chaikin")
#'   plot(st_geometry(l), col = "grey20", lwd = 2)
#'   plot(st_geometry(smoothed), col = "red", lwd = 2, add = TRUE)
#'   title("Smoothed Lines (Chaikin's Corner Cutting)", cex.main = 2, outer = TRUE)
#' }
#' par(po)
smooth <- function(x, method = c("spline", "chaikin")) {
  UseMethod("smooth")
}

#' @export
smooth.sfg <- function(x, method = c("spline", "chaikin")) {
  method <- match.arg(method)
  # choose smoother
  if (method == "spline") {
    smoother <- function(x, type) {
      smooth_spline(x = x, type = type, n_vertices = 1000)
    }
  } else if (method == "chaikin") {
    smoother <- function(x, type) {
      smooth_chaikin(x = x, type = type, refinements = 5)
    }
  } else {
    stop(paste("Invalid smoothing method:", method))
  }

  # perform smoothing
  if (sf::st_is(x, "LINESTRING")) {
    x <- sf::st_linestring(smoother(x[], type = "line"))
  } else if (sf::st_is(x, "POLYGON")) {
    for (i in seq_along(x)) {
      x[[i]] <- smoother(x[[i]], type = "polygon")
    }
  } else if (sf::st_is(x, "MULTILINESTRING")) {
    for (i in seq_along(x)) {
      x[[i]] <- smoother(x[[i]], type = "line")
    }
  } else if (sf::st_is(x, "MULTIPOLYGON")) {
    for (i in seq_along(x)) {
      for (j in seq_along(x[[i]])) {
        x[[i]][[j]] <- smoother(x[[i]][[j]], type = "polygon")
      }
    }
  } else {
    stop("smooth only valid for line and polygon features")
  }
  x
}

#' @export
smooth.sfc <- function(x, method = c("spline", "chaikin")) {
  method <- match.arg(method)
  for (i in seq_along(x)) {
    x[[i]] <- smooth(x[[i]], method = method)
  }
  sf::st_sfc(x)
}

#' @export
smooth.sf <- function(x, method = c("spline", "chaikin")) {
  method <- match.arg(method)
  sf::st_geometry(x) <- smooth(sf::st_geometry(x), method = method)
  x
}

#' @export
smooth.Spatial <- function(x, method = c("spline", "chaikin")) {
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
