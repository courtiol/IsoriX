test_that("isoscape titles do not mention any isotope by default (#155)", {
  expect_null(getOption_IsoriX("title_delta_notation"))

  german <- fixture_german() ## see helper-fixtures.R
  fit <- german$fit
  scape <- german$scape

  ## default: no isotope (and no leading space)
  expect_equal(plot(scape, plot = FALSE)$main, "mean")
  expect_equal(plot(scape, which = "mean_predVar", plot = FALSE)$main, "mean predVar")

  ## isotope defined through the global option
  old_opt <- options_IsoriX(title_delta_notation = bquote(delta^2 * H))
  on.exit(options_IsoriX(old_opt), add = TRUE)
  title <- plot(scape, plot = FALSE)$main
  expect_true(any(grepl("delta^2 * H", deparse(title), fixed = TRUE)))

  ## isotope defined through the argument y_title
  title2 <- plot(scape, y_title = list(title = bquote(delta^18 * O)), plot = FALSE)$main
  expect_true(any(grepl("delta^18 * O", deparse(title2), fixed = TRUE)))
})
