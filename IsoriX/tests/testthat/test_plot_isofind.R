test_that(".select_assign_points() keeps only the point(s) of the sample in a given panel (#103)", {
  pts <- .create_spatial_points(long = c(7, 10, 13), lat = c(48, 51, 54), ID = c("a", "b", "c"))
  ## panels showing samples "c" then "a"
  expect_equal(.select_assign_points(pts, ids = c("c", "a"), packet = 1)$ID, "c")
  expect_equal(.select_assign_points(pts, ids = c("c", "a"), packet = 2)$ID, "a")
  ## group assignment: all points
  expect_equal(nrow(.select_assign_points(pts, ids = NULL, packet = 1)), 3)
  ## objects created by older versions of IsoriX (no ID): all points
  pts_old <- .create_spatial_points(long = c(7, 10, 13), lat = c(48, 51, 54))
  expect_equal(nrow(.select_assign_points(pts_old, ids = c("c", "a"), packet = 1)), 3)
})

test_that("isofind() labels assignment points by sample and plots draw (#103)", {
  agg <- prepsources(data = GNIPDataDE)
  fit <- suppressMessages(isofit(data = agg, verbose = FALSE))
  scape <- suppressMessages(isoscape(raster = ElevRasterDE, isofit = fit, verbose = FALSE))
  d <- AssignDataAlien[1:3, ]
  assignment <- suppressWarnings(isofind(data = d, isoscape = scape, verbose = FALSE))
  expect_equal(assignment$sp_points$assigns$ID, names(assignment$sample$pv))

  old_opt <- options_IsoriX(dont_ask = TRUE)
  on.exit(options_IsoriX(old_opt), add = TRUE)
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(assignment, who = 1:3))
  expect_no_error(plot(assignment, who = 2))
  expect_no_error(plot(assignment, who = "group"))
})
