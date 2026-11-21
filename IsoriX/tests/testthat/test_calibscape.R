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
