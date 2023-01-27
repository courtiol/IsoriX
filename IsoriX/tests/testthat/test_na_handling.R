
## preparation step:
GNIPDataDEagg <- prepsources(data = GNIPDataDE)

## tests:
test_that("isofit() can handle NA", {
  GNIPDataDEagg[1, "n_source_value"] <- NA
  
  expect_error(GermanFit <<- isofit(data = GNIPDataDEagg,
                      mean_model_fix = list(elev = FALSE, lat_abs = FALSE),
                      mean_model_rand = list(uncorr = FALSE, spatial = TRUE),
                      disp_model_rand = list(uncorr = FALSE, spatial = FALSE)), regexp = NA)
})

test_that("isoscape() can handle NA", {
  ElevRasterDE[50, 50] <- NA

  expect_error(GermanScape <<- isoscape(raster = ElevRasterDE, isofit = GermanFit), regexp = NA)
})

test_that("plot.ISOSCAPE() can handle NA", {
  expect_error(plot(GermanScape), regexp = NA)
})

test_that("calibfit() can handle NA", {
  CalibDataAlien[1, "sample_value"] <- NA
  expect_error(CalibAlien <<- calibfit(data = CalibDataAlien,
                                       isofit = GermanFit,
                                       control_optim = list(maxit = 15)), regexp = NA)
})

test_that("plot.CALIBFIT can handle NA", {
  expect_error(plot(CalibAlien), regexp = NA)
})

test_that("isofind() can handle NA", {
  AssignDataAlien[1, "sample_value"] <- NA

  expect_error(AssignmentDry <<- isofind(data = AssignDataAlien,
                                         isoscape = GermanScape,
                                         calibfit = CalibAlien), regexp = NA)
  
  expect_false(all(is.na(terra::values(AssignmentDry$group$pv$lyr.1))))
  
})

test_that("plot.ISOFIND() can handle NA", {
  expect_warning(plot(AssignmentDry, who = 2), regexp = "The p-values for an assignment samples containing only missing values are considered as 0.")
  expect_error(plot(AssignmentDry, who = "group"), regexp = NA)
})

