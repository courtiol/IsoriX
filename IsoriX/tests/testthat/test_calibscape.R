test_that("calibscape() calibrates the mean isoscape of sources into sample values (#191)", {
  agg <- prepsources(data = GNIPDataDE)
  fit <- suppressMessages(isofit(data = agg, verbose = FALSE))
  scape <- suppressMessages(isoscape(raster = ElevRasterDE, isofit = fit, verbose = FALSE))
  calib <- suppressMessages(calibfit(data = data.frame(intercept = -10, slope = 0.8), method = "desk", verbose = FALSE))

  calibrated <- calibscape(isoscape = scape, calibfit = calib)

  ## class and content
  expect_s3_class(calibrated, c("ISOSCAPE", "CALIBSCAPE", "list"), exact = TRUE)
  expect_equal(names(calibrated$isoscapes), "mean")
  expect_equal(
    terra::values(calibrated$isoscapes$mean, mat = FALSE),
    -10 + 0.8 * terra::values(scape$isoscapes$mean, mat = FALSE)
  )
  expect_identical(calibrated$sp_points, scape$sp_points)

  ## summary flags the conversion
  expect_output(summary(calibrated), "has been calibrated")

  ## plotting: only the mean, with a dedicated title
  expect_equal(plot(calibrated, plot = FALSE)$main, "calibrated mean")
  expect_error(plot(calibrated, which = "mean_predVar", plot = FALSE), "must be 'mean'")

  ## no assignment, no simulation, no double conversion
  expect_error(isofind(data = AssignDataAlien[1:3, ], isoscape = calibrated, calibfit = calib, verbose = FALSE), "calibscape")
  expect_error(create_aliens(isoscape = calibrated, raster = ElevRasterDE, n_sites = 2), "calibscape")
  expect_error(calibscape(isoscape = calibrated, calibfit = calib), "already been calibrated")

  ## wrong inputs
  expect_error(calibscape(isoscape = fit, calibfit = calib), "class ISOSCAPE")
  expect_error(calibscape(isoscape = scape, calibfit = list(param = c(intercept = 1, slope = 1))), "class CALIBFIT")
})

test_that("plots of calibrated isoscapes show calibration sites instead of sources by default (#191)", {
  sources <- .create_spatial_points(long = c(7, 10, 13), lat = c(48, 51, 54))
  calibs <- .create_spatial_points(long = c(8, 11), lat = c(49, 52))
  scape <- structure(
    list(isoscapes = stats::setNames(terra::init(ElevRasterDE, "y"), "mean"), sp_points = list(sources = sources)),
    class = c("ISOSCAPE", "list")
  )
  calib <- suppressMessages(calibfit(data = data.frame(intercept = 1, slope = 1), method = "desk", verbose = FALSE))

  ## captures what plot.ISOSCAPE() asks to draw
  drawn <- NULL
  build_layers <- .build_additional_layers
  local_mocked_bindings(.build_additional_layers = function(x, sources, calibs, ...) {
    drawn <<- list(sources = sources$draw, calibs = if (is.null(calibs)) NULL else calibs$draw, calibs_pch = calibs$pch)
    build_layers(x = x, sources = sources, calibs = calibs, ...)
  })

  ## ordinary isoscapes: sources only (unchanged)
  plot(scape, plot = FALSE)
  expect_true(drawn$sources)
  expect_null(drawn$calibs)

  ## calibration method without sites ("desk"): nothing is drawn by default
  calibrated_desk <- calibscape(isoscape = scape, calibfit = calib)
  expect_null(calibrated_desk$sp_points$calibs)
  plot(calibrated_desk, plot = FALSE)
  expect_false(drawn$sources)
  expect_null(drawn$calibs)

  ## calibration method with sites: calibration sites instead of sources, with another symbol
  calib$sp_points <- list(calibs = calibs)
  calibrated <- calibscape(isoscape = scape, calibfit = calib)
  expect_identical(calibrated$sp_points$calibs, calibs)
  expect_identical(calibrated$sp_points$sources, sources)
  plot(calibrated, plot = FALSE)
  expect_false(drawn$sources)
  expect_true(drawn$calibs)
  expect_false(drawn$calibs_pch == formals(plot.ISOSCAPE)$sources$pch)

  ## the user can still ask for the sources, and hide the calibration sites
  plot(calibrated, sources = list(draw = TRUE), calibs = list(draw = FALSE), plot = FALSE)
  expect_true(drawn$sources)
  expect_false(drawn$calibs)

  ## changing other settings of the sources does not draw them
  plot(calibrated, sources = list(col = "green"), plot = FALSE)
  expect_false(drawn$sources)
})

