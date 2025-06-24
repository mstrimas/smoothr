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

  # zero threshold returns the input features unchanged
  thresh_nounits <- as.numeric(threshold)
  if (thresh_nounits < 0) {
    stop("threshold cannont be negative")
  }

  # convert threshold to crs units
  sz <- size_fxn(x[1])
  if (inherits(sz, "units")) {
    size_units <- units::set_units(1, units(sz), mode = "standard")
    threshold <- units::set_units(threshold, size_units, mode = "standard")
    stopifnot(threshold > units::set_units(0, size_units, mode = "standard"))
  }

  # assign a unique polygon id before breaking up multipart geometries
  x <- sf::st_as_sf(x, id = seq_along(x))
  all_ids <- x[["id"]]
  # split up multipart geometries
  x[["type"]] <- as.character(sf::st_geometry_type(x))
  x <- sf::st_cast(x, paste0("MULTI", geo_type), warn = FALSE)
  x <- sf::st_cast(x, geo_type, warn = FALSE)
  # apply threshold
  x <- x[size_fxn(x) > threshold, ]

  # add fully dropped features back in as empty geometries
  missing_ids <- setdiff(all_ids, x[["id"]])
  if (!drop_empty && length(missing_ids) > 0) {
    empty <- rep(sf::st_as_sfc("GEOMETRYCOLLECTION EMPTY", crs = sf::st_crs(x)),
                 length(missing_ids))
    empty <- sf::st_as_sf(empty, id = missing_ids, type = "GEOMETRYCOLLECTION")
    if (nrow(x) > 0) {
      x <- rbind(x, empty)
    } else {
      x <- empty
    }
    x <- x[order(x[["id"]]), ]
  }

  # re-group
  comb_fxn <- function(y) sf::st_cast(sf::st_combine(y), y[["type"]][1])
  x <- tapply(x, INDEX = x[["id"]], FUN = comb_fxn, simplify = FALSE)
  do.call(c, x)
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
  prj <- sp::proj4string(x)
  if (inherits(x, c("SpatialPolygonsDataFrame", "SpatialLinesDataFrame"))) {
    x_sf <- sf::st_as_sf(x)
  } else if (inherits(x, c("SpatialPolygons", "SpatialLines"))) {
    x_sf <- sf::st_as_sfc(x)
  } else{
    stop(paste("No drop_crumbs method for class", class(x)))
  }
  x_sf <- sf::st_set_crs(x_sf,  prj)

  clean <- drop_crumbs(x_sf, threshold = threshold, drop_empty = TRUE)

  if (all(sf::st_is_empty(clean))) {
    return(NULL)
  }
  clean <- sf::as_Spatial(clean)
  sp::proj4string(clean) <- prj
  return(clean)
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
