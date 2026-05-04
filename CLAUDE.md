# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package overview

`smoothr` is an R package for smoothing and tidying spatial features (lines and polygons). It is commonly used to clean up vector features converted from rasters. It supports `sf`, `sfc`, `sfg`, `sp` (`Spatial*`), and `terra` (`SpatVector`) object types.

## Common commands

```r
# Regenerate documentation and install
devtools::document()
pak::local_install()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test_smooth.R")

# Check the package (runs R CMD check)
devtools::check()

# Build the pkgdown documentation site
pkgdown::build_site()
```

## R style and formatting

- Follow the [tidyverse style guide](https://style.tidyverse.org/) for all R code.
- After writing or editing any R file, run `air format .` to auto-format. Do not manually adjust whitespace or indentation — let `air` handle it.
- Run `lintr::lint_package()` (uses `jarl` linting rules) and fix any warnings before considering a change done.

## Workflow for every feature or bug fix

1. Write or update the code in `R/`.
2. Write or update tests in `tests/testthat/`. All new features and bug fixes must have tests.
3. Run `devtools::document()` to regenerate `man/` and `NAMESPACE`.
4. Run `air format .` to format changed files.
5. Run `devtools::test()` and confirm all tests pass before finishing.
6. Update `NEWS.md` with a brief entry under the current development version.
7. Update `cran-comments.md` if the change is relevant to a CRAN submission (e.g., fixes a CRAN check failure, changes dependencies, or bumps the version).

## Architecture

The package has three layers:

1. **User-facing generics** (`smooth()`, `densify()`, `drop_crumbs()`, `fill_holes()`) — each dispatches based on the class of the input spatial object (`sf`, `sfc`, `sfg`, `Spatial*`, `SpatVector`). The dispatch logic lives in the top-level file for each function (e.g., `R/smooth.R`, `R/drop-crumbs.R`). These functions convert sp/terra inputs to sf, call the sf method, then convert back.

2. **Algorithm implementations** (`smooth_chaikin()`, `smooth_ksmooth()`, `smooth_spline()`, `smooth_densify()`) — each accepts a plain numeric matrix of coordinates and returns a smoothed matrix. These are called by the `smooth_sfc()` internal function after decomposing geometries to their component coordinate matrices.

3. **Geometry decomposition helpers** in `R/utils.R` — `add_points()` for linear interpolation, and the internal `smooth_sfc()` that iterates over geometries and component rings/linestrings.

### Key dispatch pattern

`smooth()` and `densify()` follow a consistent pattern:
- `smooth.sf` → strips geometry, calls `smooth.sfc`, re-attaches
- `smooth.sfc` → iterates geometries, calls `smooth.sfg`
- `smooth.sfg` → decomposes to coordinate matrices, applies algorithm, reconstructs

Closed loops (polygons, closed lines) are detected by comparing first and last vertices; the closure is stripped before smoothing and re-added after.

### Object type support

`terra` is in `Suggests`, not `Imports`. All terra-dispatching methods are wrapped in `rlang::check_installed("terra")` (or equivalent) to avoid hard dependency. Check `R/smooth.R` for the existing pattern before adding terra support to a new function.

## Example datasets

Three built-in datasets are used throughout tests and examples:
- `jagged_polygons` — 9 polygons with holes and multipart features (`sf`)
- `jagged_lines` — 9 lines with closed loops and multipart features (`sf`)
- `jagged_lines_3d` — lines with Z coordinates for 3D smoothing tests (`sf`)

Use these in tests; do not create synthetic test geometries unless testing an edge case not covered by these datasets.

## Testing conventions

- Test files mirror source files: `R/drop-crumbs.R` → `tests/testthat/test_drop-crumbs.R`
- Each file starts with `context(...)` then `skip_on_cran()` at file scope
- Tests check all major input types (sf, sfc, sfg, sp, SpatVector) for each function
- Use the `.R` extension (uppercase) for all R source files
