# smoothr 1.0.0

- transition to terra, remove raster dependency

# smoothr 0.2.2

- Fix errors arising from sf upgrade to 1.0-0 [issue #12]

# smoothr 0.2.1

- Fix Solaris CRAN check failure, tested and working on R-hub solaris-x86-patched

# smoothr 0.2.0

- Allow threshold = 0 in fill_holes() [issue #4]
- Provide more intuitive error message if a z-dimension is given [issue #7]
- Updates to align with recent versions of terra, e.g. no need for intermediate convrsion to sp objects
- Implemented smoothing of 3D spatial features and matrix features in any number of dimensions [issue #9]

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



