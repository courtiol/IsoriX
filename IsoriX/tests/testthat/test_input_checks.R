## Tests of the checks of the inputs of the main functions (#164).
## They use lightweight stand-in objects, since the checks happen before any
## computation, so that no model needs to be fitted.

fake <- function(class) structure(list(), class = c(class, "list"))

test_that("isofit() and isomultifit() check their data", {
  expect_error(isofit(data = fake("ISOSCAPE")), "must be a data frame.*the output of isoscape\\(\\)")
  expect_error(isofit(data = GNIPDataDE), "raw source data: use prepsources\\(\\)")
  expect_error(isofit(data = data.frame(x = 1)), "column 'mean_source_value' is missing")
  expect_error(isomultifit(data = GNIPDataDE, split_by = "month"), "raw source data")
})

test_that("isoscape() and isomultiscape() check their inputs", {
  expect_error(isoscape(raster = ElevRasterDE, isofit = fake("ISOSCAPE")), "`isofit` of isoscape\\(\\) must be the output of isofit\\(\\).*the output of isoscape\\(\\)")
  expect_error(isoscape(raster = GNIPDataDE, isofit = fake("ISOFIT")), "`raster` of isoscape\\(\\) must be a raster of class SpatRaster.*data.frame")
  expect_error(isomultiscape(raster = ElevRasterDE, isofit = fake("CALIBFIT")), "must be the output of isofit\\(\\).*the output of calibfit\\(\\)")
  expect_error(isomultiscape(raster = "elevation.tif", isofit = fake("MULTIISOFIT")), "must be a raster of class SpatRaster.*character")
})

test_that("calibfit() checks its inputs", {
  expect_error(calibfit(data = CalibDataAlien), "method 'wild' \\(default\\) requires the argument `isofit`")
  expect_error(calibfit(data = CalibDataAlien, isofit = fake("ISOSCAPE")), "must be the output of isofit\\(\\).*the output of isoscape\\(\\)")
  expect_error(calibfit(data = fake("ISOFIT"), method = "desk"), "`data` of calibfit\\(\\) must be a data frame.*the output of isofit\\(\\)")
})

test_that("isofind() checks its inputs", {
  expect_error(isofind(data = AssignDataAlien, isoscape = fake("ISOFIT")), "`isoscape` of isofind\\(\\) must be the output of isoscape\\(\\).*the output of isofit\\(\\)")
  expect_error(isofind(data = AssignDataAlien, isoscape = fake("ISOSCAPE"), calibfit = fake("ISOFIT")), "`calibfit` of isofind\\(\\) must be the output of calibfit\\(\\).*or NULL")
  expect_error(isofind(data = data.frame(sample_value = 1), isoscape = fake("ISOSCAPE")), "must contain the column\\(s\\) 'sample_ID'")
  expect_error(isofind(data = fake("CALIBFIT"), isoscape = fake("ISOSCAPE")), "`data` of isofind\\(\\) must be a data frame")
})

test_that("create_aliens() and prepraster() check their inputs", {
  expect_error(create_aliens(isoscape = fake("ISOFIT"), raster = ElevRasterDE), "`isoscape` of create_aliens\\(\\) must be the output of isoscape\\(\\)")
  expect_error(create_aliens(isoscape = fake("ISOSCAPE")), "`raster` of create_aliens\\(\\) must be a raster.*NULL")
  expect_error(prepraster(raster = fake("ISOSCAPE")), "`raster` of prepraster\\(\\) must be a raster.*the output of isoscape\\(\\)")
  expect_error(prepraster(raster = ElevRasterDE, isofit = fake("CALIBFIT")), "`isofit` of prepraster\\(\\) must be the output of isofit\\(\\).*or NULL")
})

## stand-in for fitted models: prepraster() only uses the locations of the sources
fake_fit <- structure(
  list(mean_fit = list(data = data.frame(long = c(8, 11), lat = c(49, 52)))),
  class = c("ISOFIT", "list")
)
fake_multifit <- structure(
  list(multi_fits = list(month_1 = fake_fit, month_2 = fake_fit)),
  class = c("MULTIISOFIT", "ISOFIT", "list")
)

test_that("prepraster() crops according to the sources of the output of isomultifit()", {
  cropped_fit <- prepraster(ElevRasterDE, isofit = fake_fit, verbose = FALSE)
  cropped_multifit <- prepraster(ElevRasterDE, isofit = fake_multifit, verbose = FALSE)
  expect_equal(as.vector(terra::ext(cropped_multifit)), as.vector(terra::ext(cropped_fit)))
  expect_lt(terra::ncell(cropped_multifit), terra::ncell(ElevRasterDE))
})

test_that("isomultiscape() checks the names of the layers used for weighting", {
  weighting <- c(ElevRasterDE, ElevRasterDE)
  names(weighting) <- c("a", "b")
  expect_error(
    isomultiscape(raster = ElevRasterDE, isofit = fake_multifit, weighting = weighting),
    "names of the layer in the object 'weighting' do not match"
  )
})
