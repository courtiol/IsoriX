test_that("print_nice_and_round", {
  expect_equal(.print_nice_and_round(1.222), "1.22")
  expect_equal(.print_nice_and_round(1.226), "1.23")
  expect_equal(.print_nice_and_round(0.0, digits = 1), "0.0")
  expect_equal(.print_nice_and_round(0.0, digits = 2), "0.00")
  expect_error(.print_nice_and_round(0.0, digits = -2))
})

test_that("make rasters and spatial points", {
  expect_equal(class(.create_raster(long = seq_len(100) / 10, lat = seq_len(100) / 10, values = runif(10000)))[[1]], "SpatRaster")
  expect_equal(class(.create_spatial_points(long = seq_len(100) / 10, lat = seq_len(100) / 10, values = runif(100)))[[1]], "SpatVector")
})

test_that("convert month to numbers", {
  expect_equivalent(.converts_months_to_numbers("Jan"), 1)
  expect_warning(.converts_months_to_numbers("Jo"))
})

test_that(".complete_args() completes list arguments", {
  f <- function(x = list(a = 1, b = 2)) {
    .complete_args(f)
    x
  }
  expect_equal(f(), list(a = 1, b = 2))
  expect_equal(f(list(b = 3)), list(a = 1, b = 3))
  expect_equal(f(list(5, 6)), list(a = 5, b = 6))
  expect_equal(f(list()), list(a = 1, b = 2)) ## empty list keeps all defaults (#206)
  expect_error(f(list(5)), "should contain names")
})
