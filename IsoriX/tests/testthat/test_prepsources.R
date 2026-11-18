test_that("prepsources() refuses already aggregated data", {
  expect_error(
    prepsources(data = GNIPDataEUagg, split_by = "month"),
    "already been aggregated"
  )
})

test_that("prepsources() reports missing columns", {
  d <- GNIPDataDE
  d$year <- NULL
  expect_error(prepsources(data = d), "could not be found in the data: year")
  expect_error(prepsources(data = GNIPDataDE, col_month = "mois"), "could not be found in the data: mois")
})

test_that("prepsources() still works on raw data", {
  expect_s3_class(prepsources(data = GNIPDataDE), "data.frame")
})
