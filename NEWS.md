# smoothr 0.1.2

- Importing classes from `raster` package that are required for loading the `jagged_raster` object. This is to address an email sent by Brian Ripley highlighting this issue.

# smoothr 0.1.1

- Fixed bug in `fill_holes()` and `drop_crumbs()` that was raising a warning 
when `units::set_units()` was being used. 

# smoothr 0.1.0

- Remove holes in polygons with `fill_holes()`
- Drop small lines and polygons with `drop_crumbs()`
- Kernel smoothing implemented in `smooth_ksmooth()`
- Densification of spatial lines and polygons implemented with `densify()`
- Raster example dataset added
- Vignette updated to reflect new functionality

# smoothr 0.0.1

- Initial release of smoothr



