## Objects shared by several test files.
##
## Fitting models is what makes the tests slow, so the objects that several test
## files need are created only once per test run (when first requested), instead
## of once per test file. They must not be modified by the tests using them.

.fixtures <- new.env(parent = emptyenv())

## the default isofit() model fitted on GNIPDataDE, and the corresponding isoscape
fixture_german <- function() {
  if (is.null(.fixtures$german)) {
    agg <- prepsources(data = GNIPDataDE)
    fit <- suppressMessages(isofit(data = agg, verbose = FALSE))
    scape <- suppressMessages(isoscape(raster = ElevRasterDE, isofit = fit, verbose = FALSE))
    .fixtures$german <- list(agg = agg, fit = fit, scape = scape)
  }
  .fixtures$german
}
