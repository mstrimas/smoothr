# smoothr 1.3.0

- Replace magrittr pipe `%>%` with the native R pipe `|>`, fixing a CRAN error
  caused by `sf` no longer re-exporting `%>%` (issue #20).
- Minimum R version bumped to 4.1.0 to support the native pipe.
- Remove `codecov` from `Suggests` and delete `codecov.yml`.


## Test environments

- local macOS install, R 4.5
- macOS (github actions), R 4.5
- Windows (github actions), R 4.5
- Ubuntu (github actions), R 4.5
- win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 10 reverse dependencies, comparing R CMD check results across CRAN
and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

