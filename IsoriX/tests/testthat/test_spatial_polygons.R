test_that("CountryBorders contains a projection", {
  expect_true(!is.na(slot(CountryBorders, "projargs")))
})

test_that("OceanMask contains a projection", {
  expect_true(is.na(slot(OceanMask, "projargs")))
})
