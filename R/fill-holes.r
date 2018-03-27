#' Fill small holes in polygons
#'
#' Fill polygon holes that fall below a given area threshold.
#'
#' @param x spatial polygons from either the `sf` or `sp` packages.
#' @param threshold an area threshold, below which holes will be removed.
#'   Provided either as a `units` object (see [units::set_units()]), or a
#'   numeric threshold in the units of the coordinate reference system. If `x`
#'   is in unprojected coordinates, a numeric threshold is assumed to be in
#'   square meters.
#'
#' @return A spatial feature, with holes filled, in the same format as the input
#'   data.
#' @export
#' @examples
#' # fill holes smaller than 1000km2
#' p <- jagged_polygons$geometry[5]
#' area_thresh <- units::set_units(1000, km^2)
#' p_dropped <- fill_holes(p, threshold = area_thresh)
#' # plot
#' par(mar = c(0, 0, 1, 0), mfrow = c(1, 2))
#' plot(p, col = "black", main = "Original")
#' plot(p_dropped, col = "black", main = "After fill_holes()")
fill_holes <- function(x, threshold) {
  UseMethod("fill_holes")
}

#' @export
fill_holes.sfc <- function(x, threshold) {
  stopifnot(inherits(threshold, c("units", "numeric")),
            length(threshold) == 1)
  # check geometry types and get units of feature size
  if (!all(sf::st_is(x, c("POLYGON", "MULTIPOLYGON")))) {
    stop("fill_holes() only works for polygon features.")
  }
  # convert threshold to crs units
  area_units <- units::set_units(1, sf::st_area(x[1]), mode = "standard")
  threshold <- units::set_units(threshold, area_units, mode = "standard")
  stopifnot(threshold > units::set_units(0, area_units, mode = "standard"))

  x_crs <- sf::st_crs(x)

  # loop over features, potentially multipart
  for (i in seq_along(x)) {
    # split up multipart geometries
    singles <- sf::st_cast(x[i], "POLYGON")
    # loop over single features
    for (j in seq_along(singles)) {
      # skip features with no holes
      if (length(singles[[j]]) > 1) {
        # area of holes
        a <- vapply(singles[[j]][-1], hole_area, 1, crs = x_crs)
        # assign units
        a <- units::set_units(a, area_units, mode = "standard")
        # remove holes not passing threshold
        singles[[j]] <- sf::st_polygon(singles[[j]][c(TRUE, a >= threshold)])
      }
    }
    # recombine
    if (length(singles) != 1) {
      singles <- sf::st_combine(singles)
    }
    x[[i]] <- singles[[1]]
  }
  # remove empty geometries
  sf::st_sfc(x)
}

#' @export
fill_holes.sf <- function(x, threshold) {
  sf::st_set_geometry(x, fill_holes(sf::st_geometry(x), threshold = threshold))
}

#' @export
fill_holes.Spatial <- function(x, threshold) {
  if (!requireNamespace("sp", quietly = TRUE)) {
    stop("Install the sp package to use fill_holes on sp features.")
  }
  # convert to sf object then back
  if (inherits(x, c("SpatialPolygonsDataFrame"))) {
    clean <- fill_holes(sf::st_as_sf(x), threshold = threshold)
  } else if (inherits(x, c("SpatialPolygons"))) {
    clean <- fill_holes(sf::st_as_sfc(x), threshold = threshold)
  } else {
    stop(paste("No fill_holes method for class", class(x)))
  }
  if (all(sf::st_is_empty(clean))) {
    return(NULL)
  }
  methods::as(clean, "Spatial")
}

hole_area <- function(x, crs) {
  sf::st_area(sf::st_sfc(sf::st_polygon(list(x)), crs = crs))
}
