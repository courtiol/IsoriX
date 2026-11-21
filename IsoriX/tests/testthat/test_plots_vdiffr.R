## Visual regression tests for the plotting functions, using vdiffr (#89).
##
## The plots are built from deterministic toy objects (not from fitted models),
## so that the snapshots only change if the plotting code changes, and not
## because of small numerical differences between systems in the model fits.
## To update the snapshots after an intended change, run the tests and then
## `testthat::snapshot_review()`.

testthat::local_edition(3) ## vdiffr requires the third edition of testthat
skip_if_not_installed("vdiffr")

## a small toy isoscape whose layers are simple functions of the coordinates
make_toy_isoscape <- function() {
  elev <- terra::aggregate(ElevRasterDE, fact = 10) ## 12 x 14 cells, to keep snapshots small
  elev <- terra::classify(elev, cbind(NA, 0)) ## no missing values (these cells are in the sea anyway)
  long <- terra::init(elev, "x")
  lat <- terra::init(elev, "y")
  ## (unnamed arguments, so that c() returns a SpatRaster and not a list)
  isoscapes <- c(
    -50 - 2 * (lat - 47) - 0.005 * elev, ## mean
    1 + 0.5 * abs(long - 10), ## mean_predVar
    20 + 2 * (lat - 47), ## mean_residVar
    21 + 0.5 * abs(long - 10) + 2 * (lat - 47), ## mean_respVar
    3 + 0.1 * (lat - 47), ## disp
    0.1 + 0.01 * abs(long - 10), ## disp_predVar
    0.2 + 0.01 * (lat - 47), ## disp_residVar
    0.3 + 0.01 * abs(long - 10) ## disp_respVar
  )
  names(isoscapes) <- c(
    "mean", "mean_predVar", "mean_residVar", "mean_respVar",
    "disp", "disp_predVar", "disp_residVar", "disp_respVar"
  )
  sources <- .create_spatial_points(long = c(7, 9, 10, 12, 13), lat = c(48, 53, 50, 49, 54))
  structure(list(isoscapes = isoscapes, sp_points = list(sources = sources)), class = c("ISOSCAPE", "list"))
}

toy_isoscape <- make_toy_isoscape()

## country borders and ocean mask cropped to the toy isoscape (the default ones
## cover the whole world, which would make the snapshots very large)
toy_extent <- terra::ext(toy_isoscape$isoscapes)
toy_borders <- list(borders = terra::crop(terra::readRDS(system.file("extdata/CountryBorders.rds", package = "IsoriX")), toy_extent))
toy_mask <- list(mask = terra::crop(terra::readRDS(system.file("extdata/OceanMask.rds", package = "IsoriX")), toy_extent))

## an assignment of two toy samples based on the toy isoscape (no calibration,
## so that the assignment does not involve any model fit)
toy_samples <- data.frame(
  sample_ID = c("Sample_1", "Sample_2"),
  sample_value = c(-55, -62),
  long = c(8, 12),
  lat = c(49, 53)
)
toy_assignment <- suppressWarnings(isofind(data = toy_samples, isoscape = toy_isoscape, calibfit = NULL, verbose = FALSE))

## a calibration that does not involve any model fit
toy_calibration <- calibfit(
  data = data.frame(intercept = 1.67, slope = 0.48, intercept_se = 1.65, slope_se = 0.03, resid_var = 3.96),
  method = "desk"
)

## the title is set explicitly so that these snapshots do not depend on the
## default value of the option title_delta_notation
y_title_H <- list(which = TRUE, title = bquote(delta^2 * H))

## plot.ISOSCAPE() and plot.ISOFIND() return lattice objects, which must be printed
lattice_plot <- function(x) function() print(x)

test_that("plotting isoscapes works", {
  vdiffr::expect_doppelganger(
    "isoscape mean",
    lattice_plot(plot(toy_isoscape, y_title = y_title_H, borders = toy_borders, mask = toy_mask, plot = FALSE))
  )
  vdiffr::expect_doppelganger(
    "isoscape mean_predVar",
    lattice_plot(plot(toy_isoscape, which = "mean_predVar", y_title = y_title_H, borders = toy_borders, mask = toy_mask, plot = FALSE))
  )
  vdiffr::expect_doppelganger(
    "isoscape mean custom palette no mask",
    lattice_plot(plot(toy_isoscape,
      y_title = y_title_H,
      palette = list(fn = NULL, step = 5, n_labels = 4),
      borders = toy_borders,
      mask = list(mask = NULL),
      plot = FALSE
    ))
  )
})

test_that("plotting calibrated isoscapes works (#191)", {
  ## the toy calibration (method "desk") has no calibration sites, so we add some
  toy_calibration_sites <- toy_calibration
  toy_calibration_sites$sp_points <- list(calibs = .create_spatial_points(long = c(8, 11, 14), lat = c(51, 47.5, 52)))
  toy_calibscape <- calibscape(isoscape = toy_isoscape, calibfit = toy_calibration_sites)
  ## calibration sites drawn, sources hidden
  vdiffr::expect_doppelganger(
    "calibrated isoscape mean",
    lattice_plot(plot(toy_calibscape, y_title = y_title_H, borders = toy_borders, mask = toy_mask, plot = FALSE))
  )
})

test_that("plotting assignments works", {
  vdiffr::expect_doppelganger(
    "assignment group",
    lattice_plot(plot(toy_assignment, who = "group", borders = toy_borders, mask = toy_mask, plot = FALSE))
  )
  ## each panel should only show the location of its own sample (#103)
  vdiffr::expect_doppelganger(
    "assignment individuals",
    lattice_plot(plot(toy_assignment, who = 1:2, borders = toy_borders, mask = toy_mask, plot = FALSE))
  )
})

test_that("plotting calibrations works", {
  vdiffr::expect_doppelganger(
    "calibration desk",
    function() plot(toy_calibration, xlim = c(-100, 0), ylim = c(-50, 0))
  )
})

test_that("plotting SpatRaster works", {
  vdiffr::expect_doppelganger(
    "spatraster elevation",
    function() plot(terra::aggregate(ElevRasterDE, fact = 10))
  )
})
