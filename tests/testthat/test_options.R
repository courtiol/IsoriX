test_that("test options modification and retrieval", {
  options_IsoriX(example_maxtime = 30)
  expect_equal(getOption_IsoriX("example_maxtime"), 30)
  expect_equal(getOption_IsoriX("Ncpu"), 2)
  expect_equal(getOption_IsoriX("spaMM_debug"), FALSE)
  expect_equal(getOption_IsoriX("spaMM_blocksize"), 16000L)
  expect_equal(length(getOption_IsoriX()), 6L)
  expect_error(getOption_IsoriX("bidon"))
  expect_error(getOption_IsoriX(c("example_maxtime", "bidon")))
})

test_that("the option spaMM_blocksize is validated and used by isoscape()", {
  old <- options_IsoriX()
  withr::defer(options_IsoriX(old))

  options_IsoriX(spaMM_blocksize = 500)
  expect_identical(getOption_IsoriX("spaMM_blocksize"), 500L)

  for (bad in list(0, -1, 2.5, NA, "1000", c(1000, 2000))) {
    options_IsoriX(spaMM_blocksize = bad)
    expect_error(getOption_IsoriX("spaMM_blocksize"), "spaMM_blocksize")
  }

  ## a smaller block size leads to the same isoscape
  options_IsoriX(old)
  german <- fixture_german()
  options_IsoriX(spaMM_blocksize = 3000L)
  scape <- suppressMessages(
    isoscape(raster = ElevRasterDE, isofit = german$fit, verbose = FALSE)
  )
  expect_equal(
    terra::values(scape$isoscapes),
    terra::values(german$scape$isoscapes)
  )
})
