## Tests for plot_sources() (#195)

test_that("plot_sources() returns one map per statistic", {
  agg <- prepsources(GNIPDataDE)
  maps <- plot_sources(agg, plot = FALSE)
  expect_named(maps, c("mean", "sd", "n"))
  expect_true(all(vapply(maps, inherits, logical(1), what = "trellis")))
  expect_equal(vapply(maps, function(map) map$main, character(1)), c(mean = "Mean", sd = "SD", n = "N"))

  maps <- plot_sources(agg, which = c("n", "mean"), plot = FALSE)
  expect_named(maps, c("n", "mean"))

  ## counts are shown without decimals in the colour key
  key_labels <- maps$n$legend$right$args$key$labels$labels
  expect_false(any(grepl(".", key_labels, fixed = TRUE)))
})

test_that("plot_sources() draws points, labels and works without borders", {
  agg <- prepsources(GNIPDataDE)
  pdf(NULL)
  withr::defer(dev.off())
  expect_no_error(plot_sources(agg))
  expect_no_error(plot_sources(agg, labels = TRUE))
  expect_no_error(plot_sources(agg, which = "n", borders = list(borders = NULL), palette = list(fn = NULL)))
  expect_no_error(plot_sources(tibble::as_tibble(agg), which = "mean"))
  ## no prompt on non-interactive devices, and the setting of the device is kept
  expect_false(grDevices::devAskNewPage())
})

test_that("plot_sources() stops with informative messages on wrong inputs", {
  expect_error(plot_sources(GNIPDataDE), "prepsources")
  expect_error(plot_sources(prepsources(GNIPDataDE), which = "median"), "should be one of")
  monthly <- prepsources(GNIPDataDE, split_by = "month")
  expect_error(plot_sources(monthly, plot = FALSE), "single row per source location")
  expect_no_error(plot_sources(monthly[monthly$month == 1, ], plot = FALSE))
})

test_that("plot_sources() handles sources straddling the International Date Line", {
  agg <- data.frame(
    source_ID = c("a", "b", "c"),
    long = c(170, 179, -170),
    lat = c(-10, 0, 10),
    mean_source_value = c(-10, -20, -30),
    var_source_value = c(1, 2, 3),
    n_source_value = c(5, 6, 7)
  )
  maps <- plot_sources(agg, plot = FALSE)
  expect_true(all(maps$mean$x.limits > 160 & maps$mean$x.limits < 200))
})
