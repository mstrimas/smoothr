#' Remove small polygons or line segments
#'
#' Remove polygons or line segments below a given area or length threshold.
#'
#' For multipart features, the removal threshold is applied to the individual
#' components. This means that, in some cases, an entire feature may be removed,
#' while in other cases, only parts of the multipart feature will be removed.
#'
#' @inheritParams smooth
#' @param threshold an area or length threshold, below which features will be
#'   removed. Provided either as a `units` object (see [units::set_units()]), or
#'   a numeric threshold in the units of the coordinate reference system. If `x`
#'   is in unprojected coordinates, a numeric threshold is assumed to be in
#'   meters.
#' @param drop_empty logical; whether features with sizes below the given
#'   threshold should be removed (the default) or kept as empty geometries. Note
#'   that `sp` objects cannot store empty geometries, so this argument will be
#'   ignored and empty geometries will always be removed.
#'
#' @return A spatial feature, with small pieces removed, in the same format as
#'   the input data. If none of the features are larger than the threshold,
#'   `sf` inputs will return a geometry set with zero features, and `sp` inputs
#'   will return `NULL`.
#' @export
#' @examples
#' # remove polygons smaller than 200km2
#' p <- jagged_polygons$geometry[7]
#' area_thresh <- units::set_units(200, km^2)
#' p_dropped <- drop_crumbs(p, threshold = area_thresh)
#' # plot
#' par(mar = c(0, 0, 1, 0), mfrow = c(1, 2))
#' plot(p, col = "black", main = "Original")
#' if (length(p_dropped) > 0) {
#'   plot(p_dropped, col = "black", main = "After drop_crumbs()")
#' }
#'
#'
#' # remove lines less than 25 miles
#' l <- jagged_lines$geometry[8]
#' # note that any units can be used
#' # conversion to units of projection happens automatically
#' length_thresh <- units::set_units(25, miles)
#' l_dropped <- drop_crumbs(l, threshold = length_thresh)
#' # plot
#' par(mar = c(0, 0, 1, 0), mfrow = c(1, 2))
#' plot(l, lwd = 5, main = "Original")
#' if (length(l_dropped)) {
#'   plot(l_dropped, lwd = 5, main = "After drop_crumbs()")
#' }
#'
drop_crumbs <- function(x, threshold, drop_empty = TRUE) {
  UseMethod("drop_crumbs")
}

#' @export
drop_crumbs.sfc <- function(x, threshold, drop_empty = TRUE) {
  stopifnot(inherits(threshold, c("units", "numeric")),
            length(threshold) == 1)
  # check geometry types and get units of feature size
  if (all(sf::st_is(x, c("POLYGON", "MULTIPOLYGON")))) {
    size_fxn <- sf::st_area
    geo_type <- "POLYGON"
  } else if (all(sf::st_is(x, c("LINESTRING", "MULTILINESTRING")))) {
    size_fxn <- sf::st_length
    geo_type <- "LINESTRING"
  } else {
    stop(paste("drop_crumbs() only works for line and polygon features",
               "and geometry types cannot be mixed."))
  }

  # convert threshold to crs units
  size_units <- units::set_units(1, units(size_fxn(x[1])), mode = "standard")
  threshold <- units::set_units(threshold, size_units, mode = "standard")
  stopifnot(threshold > units::set_units(0, size_units, mode = "standard"))

  # loop over features
  for (i in seq_along(x)) {
    # split up multipart geometries
    singles <- sf::st_cast(x[i], geo_type)
    # test threshold
    passed <- singles[size_fxn(singles) >= threshold]
    # recombine
    if (length(passed) != 1) {
      passed <- sf::st_combine(passed)
    }
    x[[i]] <- passed[[1]]
  }
  # remove empty geometries
  x <- sf::st_sfc(x)
  if (drop_empty) {
    x <- x[!sf::st_is_empty(x)]
  }
  x
}

#' @export
drop_crumbs.sf <- function(x, threshold, drop_empty = TRUE) {
  g <- drop_crumbs(sf::st_geometry(x), threshold = threshold,
                   drop_empty = FALSE)
  sf::st_geometry(x) <- g
  if (drop_empty) {
    x <- x[!sf::st_is_empty(x), ]
  }
  x
}

#' @export
drop_crumbs.Spatial <- function(x, threshold, drop_empty = TRUE) {
  if (!requireNamespace("sp", quietly = TRUE)) {
    stop("Install the sp package to use drop_crumbs on sp features.")
  }
  # convert to sf object then back
  if (inherits(x, c("SpatialPolygonsDataFrame", "SpatialLinesDataFrame"))) {
    clean <- drop_crumbs(sf::st_as_sf(x), threshold = threshold,
                         drop_empty = TRUE)
  } else if (inherits(x, c("SpatialPolygons", "SpatialLines"))) {
    clean <- drop_crumbs(sf::st_as_sfc(x), threshold = threshold,
                         drop_empty = TRUE)
  } else{
    stop(paste("No drop_crumbs method for class", class(x)))
  }
  if (all(sf::st_is_empty(clean))) {
    return(NULL)
  }
  methods::as(clean, "Spatial")
}

#' @export
drop_crumbs.SpatVector <- function(x, threshold, drop_empty = TRUE) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Install the terra package to process SpatVector features.")
  }
  warning("SpatVector objects are converted to sf objects in smoothr. ",
          "This conversion may introduce errors and increase the time ",
          "required to perform smoothing.")
  clean <- drop_crumbs(sf::st_as_sf(x), threshold = threshold,
                       drop_empty = TRUE)
  terra::vect(clean)
}
