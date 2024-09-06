test_that("test options modification and retrieval", {
  options_IsoriX(example_maxtime = 30)
  expect_equal(getOption_IsoriX("example_maxtime"), 30)
  expect_equal(getOption_IsoriX("Ncpu"), 2)
  expect_equal(getOption_IsoriX("spaMM_debug"), FALSE)
  expect_equal(length(getOption_IsoriX()), 5L)
  expect_error(getOption_IsoriX("bidon"))
  expect_error(getOption_IsoriX(c("example_maxtime", "bidon")))
})
