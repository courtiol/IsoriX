## isoscape() computes all predictions in one call per model, letting spaMM
## slice the computation of the prediction variances (#150, #176).
## These tests check that the predictions stored in the isoscape are those that
## spaMM produces for each location, including when some locations have NA
## predictors and when the raster has more cells than spaMM's slices.

test_that("isoscape() stores, for each cell, the predictions from spaMM", {
  agg <- prepsources(data = GNIPDataDE)
  fit <- suppressMessages(isofit(
    data = agg,
    mean_model_fix = list(elev = TRUE, lat_abs = TRUE), ## elev for NA to matter
    verbose = FALSE
  ))

  raster <- ElevRasterDE
  values <- terra::values(raster)
  na_cells <- seq(1L, length(values), by = 7L)
  values[na_cells] <- NA
  raster <- terra::setValues(raster, values)
  expect_gt(terra::ncell(raster), .blocksize_isoscape) ## several slices

  scape <- suppressMessages(isoscape(raster = raster, isofit = fit, verbose = FALSE))
  iso <- terra::values(scape$isoscapes)

  ## NA in the structural raster lead to NA in the mean isoscapes, and only
  ## there; the dispersion model does not depend on elevation, so it is
  ## predicted everywhere
  expect_identical(which(is.na(iso[, "mean"])), na_cells)
  expect_identical(which(is.na(iso[, "mean_predVar"])), na_cells)
  expect_false(anyNA(iso[, "disp"]))

  ## predictions match direct calls to spaMM on the locations without NA
  xs <- attr(scape, "xs")
  ok <- !is.na(xs$elev)
  xs_ok <- xs[ok, ]
  pred_disp <- suppressWarnings(spaMM::predict.HLfit(fit$disp_fit,
    newdata = xs_ok, variances = list(respVar = TRUE)
  ))
  xs_ok$pred_disp <- pred_disp[, 1]
  pred_mean <- suppressWarnings(spaMM::predict.HLfit(fit$mean_fit,
    newdata = xs_ok, variances = list(respVar = TRUE)
  ))

  expect_equal(iso[ok, "mean"], unname(c(pred_mean[, 1])))
  expect_equal(iso[ok, "mean_predVar"], unname(c(attr(pred_mean, "predVar"))))
  expect_equal(iso[ok, "mean_respVar"], unname(c(attr(pred_mean, "respVar"))))
  expect_equal(iso[ok, "disp"], unname(c(pred_disp[, 1])))
  expect_equal(iso[ok, "disp_predVar"], unname(c(attr(pred_disp, "predVar"))))
})

test_that("isoscape() runs with verbose = TRUE", {
  fit <- fixture_german()$fit
  expect_error(
    suppressMessages(utils::capture.output(isoscape(raster = ElevRasterDE, isofit = fit, verbose = TRUE))),
    regexp = NA
  )
})
