## the objects saved and read are the shared ones (see helper-fixtures.R)


test_that("saveRDS() and readRDS() can handle ISOFIT objects", {
  tempfile_fit <- tempfile(fileext = ".rds")
  GermanFit <<- fixture_german()$fit
  suppressWarnings(saveRDS(GermanFit, file = tempfile_fit)) ## silly RStudio warning "'package:stats' may not be available when loading"
  GermanFit2 <- readRDS(tempfile_fit)
  expect_equal(spaMM::fixef(GermanFit$mean_fit), spaMM::fixef(GermanFit2$mean_fit))
})

test_that("saveRDS() and readRDS() can handle ISOSCAPE objects", {
  tempfile_isoscape <- tempfile(fileext = ".rds")
  GermanScape <<- fixture_german()$scape
  saveRDS(GermanScape, file = tempfile_isoscape)
  GermanScape2 <- readRDS(tempfile_isoscape)
  expect_s3_class(GermanScape2, "ISOSCAPE")
  expect_equal(names(GermanScape2$isoscapes), names(GermanScape$isoscapes))
  expect_equal(terra::values(GermanScape2$isoscapes), terra::values(GermanScape$isoscapes))
  expect_equal(terra::geom(GermanScape2$sp_points$sources), terra::geom(GermanScape$sp_points$sources))
})

test_that("saveRDS() and readRDS() can handle ISOFIND objects", {
  tempfile_isofind <- tempfile(fileext = ".rds")
  suppressWarnings(AssignmentDry <<- isofind(data = AssignDataAlien, isoscape = GermanScape, calibfit = NULL))
  saveRDS(AssignmentDry, file = tempfile_isofind)
  AssignmentDry2 <- readRDS(tempfile_isofind)
  ## (terra objects are compared through their content, since comparing them
  ## directly also compares their internal pointers, which differ once reloaded)
  expect_s3_class(AssignmentDry2, "ISOFIND")
  for (part in c("sample", "group")) {
    expect_equal(names(AssignmentDry2[[part]]), names(AssignmentDry[[part]]))
    for (layer in names(AssignmentDry[[part]])) {
      expect_equal(terra::values(AssignmentDry2[[part]][[layer]]), terra::values(AssignmentDry[[part]][[layer]]))
    }
  }
  expect_equal(terra::geom(AssignmentDry2$sp_points$assigns), terra::geom(AssignmentDry$sp_points$assigns))
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
