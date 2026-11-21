## Tests for the handling of the antimeridian (International Date Line) (#180)

world_raster <- function(vals = 1) {
  terra::rast(
    nrows = 180, ncols = 360, xmin = -180, xmax = 180, ymin = -90, ymax = 90,
    crs = "+proj=longlat +datum=WGS84", vals = vals
  )
}

test_that(".long_straddles_antimeridian() detects locations straddling the antimeridian", {
  expect_true(.long_straddles_antimeridian(c(170, 179, -179, -170)))
  expect_true(.long_straddles_antimeridian(c(70, 180, -150)))
  expect_false(.long_straddles_antimeridian(c(5, 10, 15)))
  expect_false(.long_straddles_antimeridian(c(-170, -10, 10, 170))) ## world-wide
  expect_false(.long_straddles_antimeridian(c(170, 190))) ## already within [0, 360]
  expect_false(.long_straddles_antimeridian(c(NA, 179)))
})

test_that(".wrap_long_like() expresses longitudes like the reference ones", {
  expect_equal(.wrap_long_like(c(-170, 10, 170, NA), long_ref = c(100, 200)), c(190, 10, 170, NA))
  expect_equal(.wrap_long_like(c(190, 10, -170, NA), long_ref = c(-10, 10)), c(-170, 10, -170, NA))
  expect_null(.wrap_long_like(NULL, long_ref = c(100, 200)))
})

test_that(".select_long() selects longitudes across the antimeridian", {
  long <- c(-180, -160, -150, -100, 0, 69, 70, 120, 180)
  expect_equal(.select_long(long, -180, 180), rep(TRUE, length(long)))
  expect_equal(.select_long(long, -10, 100), long >= -10 & long <= 100)
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
  expect_equal(.select_long(long, 70, -150), expected)
  expect_equal(.select_long(long, 70, 210), expected)
})

test_that("prepsources() can select sources across the antimeridian", {
  data <- data.frame(
    source_ID = c("west", "dateline", "east", "far"),
    long = c(-160, 179, 120, 0),
    lat = 0,
    source_value = -10
  )
  agg <- prepsources(data, long_min = 70, long_max = -150)
  expect_setequal(as.character(agg$source_ID), c("west", "dateline", "east"))
})

test_that(".crop_long() crops rasters across the antimeridian", {
  raster <- world_raster(vals = seq_len(64800))
  crop1 <- .crop_long(raster, xmin = 70, xmax = -150, ymin = -50, ymax = 70)
  crop2 <- .crop_long(raster, xmin = 70, xmax = 210, ymin = -50, ymax = 70)
  expect_equal(as.vector(terra::ext(crop1)), c(xmin = 70, xmax = 210, ymin = -50, ymax = 70))
  expect_identical(terra::values(crop1), terra::values(crop2))
  ## values east of the antimeridian come from the original (western) cells
  expect_equal(
    terra::extract(crop1, cbind(200.5, 10.5))[[1]],
    terra::extract(raster, cbind(-159.5, 10.5))[[1]]
  )
  ## usual crop are unchanged
  expect_identical(
    terra::values(.crop_long(raster, xmin = -10, xmax = 30, ymin = 0, ymax = 50)),
    terra::values(terra::crop(raster, terra::ext(-10, 30, 0, 50)))
  )
})

test_that("prepraster() crops rasters across the antimeridian without warning", {
  raster <- world_raster()
  expect_no_warning(r <- prepraster(raster, manual_crop = c(70, -150, -50, 70), verbose = FALSE))
  expect_equal(as.vector(terra::ext(r)), c(xmin = 70, xmax = 210, ymin = -50, ymax = 70))
})

test_that(".wrap_vector_to_extent() and .wrap_points_to_extent() shift spatial objects", {
  extent <- terra::ext(70, 210, -50, 70)
  borders <- .wrap_vector_to_extent(CountryBorders, extent)
  expect_equal(nrow(borders), 2 * nrow(CountryBorders))
  expect_identical(.wrap_vector_to_extent(CountryBorders, terra::ext(-10, 30, 0, 50)), CountryBorders)

  points <- .create_spatial_points(long = c(-170, 150, 0), lat = c(0, 10, 20), ID = c("a", "b", "c"))
  points_wrapped <- .wrap_points_to_extent(points, extent)
  expect_equal(terra::crds(points_wrapped)[, 1], c(190, 150, 0))
  expect_equal(points_wrapped$ID, c("a", "b", "c"))
})

test_that("the full workflow works across the antimeridian", {
  skip_on_cran()
  set.seed(123)
  n <- 30
  sources <- data.frame(
    source_ID = paste0("s", seq_len(n)),
    long = c(runif(20, 110, 180), runif(10, -180, -160)),
    lat = runif(n, -40, 50)
  )
  sources$source_value <- -2 * abs(sources$lat) + rnorm(n)
  sources <- sources[rep(seq_len(n), each = 2), ]
  sources$source_value <- sources$source_value + rnorm(nrow(sources))
  agg <- prepsources(sources, long_min = 100, long_max = -150)

  expect_message(
    fit <- isofit(agg, mean_model_fix = list(lat_abs = TRUE, long = TRUE), verbose = FALSE),
    "antimeridian"
  )
  expect_true(all(fit$mean_fit$data$long >= 0))

  raster <- suppressWarnings(prepraster(world_raster(vals = 0), isofit = fit, aggregation_factor = 2, verbose = FALSE))
  expect_gt(terra::xmax(raster), 180)

  iso <- suppressMessages(isoscape(raster, fit, verbose = FALSE))
  expect_false(all(is.na(terra::values(iso$isoscapes$mean))))

  calib <- data.frame(
    site_ID = rep(paste0("c", 1:8), each = 3),
    long = rep(c(runif(5, 120, 179), runif(3, -179, -165)), each = 3),
    lat = rep(runif(8, -30, 40), each = 3),
    elev = 0,
    species_ID = "sp",
    sample_ID = paste0("x", 1:24)
  )
  calib$sample_value <- -2.2 * abs(calib$lat) + rnorm(nrow(calib))
  cal <- suppressMessages(calibfit(calib, isofit = fit, verbose = FALSE))
  expect_true(all(cal$data$long >= 0))

  assign <- data.frame(sample_ID = c("a", "b"), sample_value = c(-40, -60), long = c(-170, 150), lat = c(10, 30))
  ass <- suppressMessages(isofind(assign, isoscape = iso, calibfit = cal, verbose = FALSE))
  expect_equal(terra::crds(ass$sp_points$assigns)[, 1], c(190, 150))

  ## plots
  expect_no_error(print(plot(iso)))
  expect_no_error(suppressWarnings(print(plot(ass, who = 1:2))))
})
