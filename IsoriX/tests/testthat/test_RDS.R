## preparation step:
GNIPDataDEagg <- prepsources(data = GNIPDataDE)

## tests:

test_that("saveRDS() and readRDS() can handle ISOFIT objects", {
  tempfile_fit <- tempfile(fileext = ".rds")
  GermanFit <<- isofit(data = GNIPDataDEagg)
  suppressWarnings(saveRDS(GermanFit, file = tempfile_fit)) ## silly RStudio warning "'package:stats' may not be available when loading"
  GermanFit2 <- readRDS(tempfile_fit)
  expect_equal(spaMM::fixef(GermanFit$mean_fit), spaMM::fixef(GermanFit2$mean_fit))
})

test_that("saveRDS() and readRDS() can handle ISOSCAPE objects", {
  tempfile_isoscape <- tempfile(fileext = ".rds")
  GermanScape <<- isoscape(raster = ElevRasterDE, isofit = GermanFit)
  saveRDS(GermanScape, file = tempfile_isoscape)
  GermanScape2 <- readRDS(tempfile_isoscape)
  expect_equal(GermanScape2$mean_scape, GermanScape$mean_scape)
})

test_that("saveRDS() and readRDS() can handle ISOFIND objects", {
  tempfile_isofind <- tempfile(fileext = ".rds")
  suppressWarnings(AssignmentDry <<- isofind(data = AssignDataAlien, isoscape = GermanScape, calibfit = NULL))
  saveRDS(AssignmentDry, file = tempfile_isofind)
  AssignmentDry2 <- readRDS(tempfile_isofind)
  expect_equal(AssignmentDry2, AssignmentDry)
})

test_that("saveRDS() and readRDS() can handle SpatRaster objects", {
  tempfile_spatraster <- tempfile(fileext = ".rds")
  f <- system.file("ex/elev.tif", package = "terra")
  r <- terra::rast(f)
  saveRDS(r, file = tempfile_spatraster)
  r2 <- readRDS(tempfile_spatraster)
  expect_equal(values(r2), values(r))
})

test_that("saveRDS() and readRDS() can handle data.frame objects", {
  tempfile_dataframe <- tempfile(fileext = ".rds")
  saveRDS(iris, file = tempfile_dataframe)
  iris2 <- readRDS(tempfile_dataframe)
  expect_equal(iris2, iris)
})
