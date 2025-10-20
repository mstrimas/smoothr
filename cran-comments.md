## Resubmission

- removed `jagged_raster` `terra` object to allow `terra` to be moved to Suggests, then replaced with a GeoTIFF file in `inst/extdata/jagged-raster.tif`

# smoothr 1.2.1

- move terra to Suggests (issue #19)

## Test environments

- local OS X install, R 4.5
- OS X (github actions), R 4.5
- Windows (github actions), R 4.5
- ubuntu 14.04 (github actions), R 4.5
- win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 9 reverse dependencies (8 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

