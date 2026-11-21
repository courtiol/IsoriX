test_that("isotranscape() converts the mean isoscape of sources into sample values (#191)", {
  agg <- prepsources(data = GNIPDataDE)
  fit <- suppressMessages(isofit(data = agg, verbose = FALSE))
  scape <- suppressMessages(isoscape(raster = ElevRasterDE, isofit = fit, verbose = FALSE))
  calib <- suppressMessages(calibfit(data = data.frame(intercept = -10, slope = 0.8), method = "desk", verbose = FALSE))

  rescaled <- isotranscape(isoscape = scape, calibfit = calib)

  ## class and content
  expect_s3_class(rescaled, c("ISOSCAPE", "ISORESCALED", "list"), exact = TRUE)
  expect_equal(names(rescaled$isoscapes), "mean")
  expect_equal(
    terra::values(rescaled$isoscapes$mean, mat = FALSE),
    -10 + 0.8 * terra::values(scape$isoscapes$mean, mat = FALSE)
  )
  expect_identical(rescaled$sp_points, scape$sp_points)

  ## summary flags the conversion
  expect_output(summary(rescaled), "converted into sample values")

  ## plotting: only the mean, with a dedicated title
  expect_equal(plot(rescaled, plot = FALSE)$main, "rescaled mean")
  expect_error(plot(rescaled, which = "mean_predVar", plot = FALSE), "must be 'mean'")

  ## no assignment, no simulation, no double conversion
  expect_error(isofind(data = AssignDataAlien[1:3, ], isoscape = rescaled, calibfit = calib, verbose = FALSE), "isotranscape")
  expect_error(create_aliens(isoscape = rescaled, raster = ElevRasterDE, n_sites = 2), "isotranscape")
  expect_error(isotranscape(isoscape = rescaled, calibfit = calib), "already been converted")

  ## wrong inputs
  expect_error(isotranscape(isoscape = fit, calibfit = calib), "class ISOSCAPE")
  expect_error(isotranscape(isoscape = scape, calibfit = list(param = c(intercept = 1, slope = 1))), "class CALIBFIT")
})
