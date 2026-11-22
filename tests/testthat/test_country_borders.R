test_that("CountryBorders contains the names of the countries (#183)", {
  borders <- terra::readRDS(system.file("extdata/CountryBorders.rds", package = "IsoriX"))
  expect_identical(names(borders), "name")
  expect_false(anyNA(borders$name))
  expect_false(anyDuplicated(borders$name) > 0)
  expect_equal(nrow(borders), 242)

  ## a country can be selected by name
  germany <- borders[borders$name == "Germany", ]
  expect_equal(nrow(germany), 1)
  ext_germany <- as.vector(terra::ext(germany))
  expect_true(ext_germany[["xmin"]] > 5 && ext_germany[["xmax"]] < 16)
  expect_true(ext_germany[["ymin"]] > 47 && ext_germany[["ymax"]] < 56)
})

test_that("OceanMask is the complement of CountryBorders", {
  borders <- terra::readRDS(system.file("extdata/CountryBorders.rds", package = "IsoriX"))
  ocean <- terra::readRDS(system.file("extdata/OceanMask.rds", package = "IsoriX"))
  expect_equal(nrow(ocean), 1)
  ## a point in the North Sea is in the ocean mask, a point in Berlin is not
  pts <- terra::vect(data.frame(long = c(3, 13.4), lat = c(56, 52.5)), geom = c("long", "lat"), crs = terra::crs(ocean))
  expect_identical(terra::relate(pts, ocean, "intersects")[, 1], c(TRUE, FALSE))
  expect_identical(borders$name[terra::relate(pts[2], borders, "intersects")[1, ]], "Germany")
})
