test_that("printing IsoriX objects displays no stray NULL (#220)", {
  german <- fixture_german() ## see helper-fixtures.R
  fit <- german$fit
  scape <- german$scape
  assignment <- suppressWarnings(isofind(data = AssignDataAlien[1:2, ], isoscape = scape, verbose = FALSE))
  set.seed(1)
  aliens <- create_aliens(isoscape = scape, raster = ElevRasterDE, n_sites = 10, min_n_samples = 3, max_n_samples = 3)
  calib_lab <- suppressMessages(calibfit(data = aliens, method = "lab", verbose = FALSE))

  for (obj in list(fit, scape, assignment, calib_lab)) {
    out <- capture.output(res <- print(obj))
    expect_false(any(trimws(out) == "NULL"))
    expect_identical(res, obj) ## print methods return their input invisibly
    expect_false(any(trimws(capture.output(summary(obj))) == "NULL"))
  }

  ## the spaMM summaries are still displayed
  expect_true(any(grepl("Likelihood values", capture.output(print(fit)))))
  expect_true(any(grepl("Likelihood values", capture.output(summary(calib_lab)))))

  ## the source points are still displayed for isoscapes
  expect_true(any(grepl("SpatVector", capture.output(print(scape)))))
})
