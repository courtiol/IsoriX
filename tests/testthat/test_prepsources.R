test_that("prepsources() refuses already aggregated data", {
  expect_error(
    prepsources(data = GNIPDataEUagg, split_by = "month"),
    "already been aggregated"
  )
})

test_that("prepsources() reports missing required columns", {
  d <- GNIPDataDE
  d$lat <- NULL
  expect_error(prepsources(data = d), "could not be found in the data: lat")
})

test_that("prepsources() still works on raw data", {
  expect_s3_class(prepsources(data = GNIPDataDE), "data.frame")
})

test_that("prepsources() works without year and month when not filtering on them", {
  ref <- prepsources(data = GNIPDataDE)
  d <- GNIPDataDE
  d$year <- NULL
  d$month <- NULL
  res <- prepsources(data = d)
  expect_equal(res, ref)
})

test_that("prepsources() requires year or month only when they are used", {
  d <- GNIPDataDE
  d$year <- NULL
  d$month <- NULL
  expect_error(prepsources(data = d, year = 2000), "could not be found in the data: year")
  expect_error(prepsources(data = d, split_by = "year"), "could not be found in the data: year")
  expect_error(prepsources(data = d, month = 1:6), "could not be found in the data: month")
  expect_error(prepsources(data = d, split_by = "month"), "could not be found in the data: month")
  d2 <- GNIPDataDE
  d2$year <- NULL
  expect_equal(
    prepsources(data = d2, month = 5:8),
    prepsources(data = GNIPDataDE, month = 5:8)
  )
})

test_that("prepsources() works without elevation", {
  ref <- prepsources(data = GNIPDataDE)
  d <- GNIPDataDE
  d$elev <- NULL
  res <- prepsources(data = d)
  expect_false("elev" %in% colnames(res))
  ref$elev <- NULL
  expect_equal(res, ref)
})

test_that("isofit() works if elevation is absent from both the data and the model", {
  d <- GNIPDataDE
  d$elev <- NULL
  agg <- prepsources(data = d)
  expect_false("elev" %in% colnames(agg))
  fit <- isofit(
    data = agg,
    mean_model_fix = list(elev = FALSE, lat_abs = TRUE),
    disp_model_fix = list(elev = FALSE),
    verbose = FALSE
  )
  expect_s3_class(fit, "ISOFIT")
  expect_false("elev" %in% all.vars(stats::formula(fit$mean_fit)))
  expect_false("elev" %in% all.vars(stats::formula(fit$disp_fit)))
})

test_that("isofit() stops if elevation is requested but absent", {
  d <- GNIPDataDE
  d$elev <- NULL
  agg <- prepsources(data = d)
  expect_error(
    isofit(data = agg, mean_model_fix = list(elev = TRUE)),
    "Elevation is used as a predictor"
  )
})
