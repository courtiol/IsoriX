## Tests for the handling of the antimeridian (International Date Line) (#180)

world_raster <- function(vals = 1) {
  terra::rast(
    nrows = 180, ncols = 360, xmin = -180, xmax = 180, ymin = -90, ymax = 90,
    crs = "+proj=longlat +datum=WGS84", vals = vals
  )
}

test_that(".make_long_continuous() only changes sources straddling the antimeridian", {
  long_after <- function(long) suppressMessages(.make_long_continuous(data.frame(long = long))$long)
  expect_message(.make_long_continuous(data.frame(long = c(170, -170))), "antimeridian")
  expect_equal(long_after(c(170, 179, -179, -170)), c(170, 179, 181, 190))
  expect_equal(long_after(c(70, 180, -150)), c(70, 180, 210))
  expect_equal(long_after(c(5, 10, 15)), c(5, 10, 15))
  expect_equal(long_after(c(-170, -10, 10, 170)), c(-170, -10, 10, 170)) ## world-wide
  expect_equal(long_after(c(170, 190)), c(170, 190)) ## already continuous
  expect_equal(long_after(c(NA, 179)), c(NA, 179))
})

test_that(".continuous_long_range() expresses ranges of longitudes continuously", {
  expect_equal(.continuous_long_range(70, -150), c(70, 210))
  expect_equal(.continuous_long_range(70, 210), c(70, 210))
  expect_equal(.continuous_long_range(-200, -150), c(160, 210))
  expect_equal(.continuous_long_range(180, -150), c(-180, -150))
  expect_equal(.continuous_long_range(-180, 180), c(-180, 180))
  expect_equal(.continuous_long_range(-10, 30), c(-10, 30))
  expect_equal(.continuous_long_range(-150, -100, from = 0), c(210, 260))
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
  expect_equal(.select_long(long, -290, -150), expected)
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
  expect_equal(terra::values(crop1), terra::values(crop2))
  expect_false(anyNA(terra::values(crop1)))
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

test_that(".wrap_vector_to_extent() adds shifted copies of polygons and points", {
  extent <- terra::ext(70, 210, -50, 70)
  borders <- .wrap_vector_to_extent(CountryBorders, extent)
  expect_equal(nrow(borders), 2 * nrow(CountryBorders))
  expect_identical(.wrap_vector_to_extent(CountryBorders, terra::ext(-10, 30, 0, 50)), CountryBorders)
  expect_null(.wrap_vector_to_extent(NULL, extent))

  points <- .create_spatial_points(long = c(-170, 150), lat = c(0, 10), ID = c("a", "b"))
  points_wrapped <- .wrap_vector_to_extent(points, extent)
  expect_equal(terra::crds(points_wrapped)[, 1], c(-170, 150, 190, 510))
  expect_equal(points_wrapped$ID, c("a", "b", "a", "b")) ## the IDs are kept (#103)
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
  cal <- suppressMessages(calibfit(calib, isofit = fit, control_optim = list(maxit = 1), verbose = FALSE)) ## capped to save time
  expect_true(all(cal$data$long >= 0))

  assign <- data.frame(sample_ID = c("a", "b"), sample_value = c(-40, -60), long = c(-170, 150), lat = c(10, 30))
  ass <- suppressMessages(isofind(assign, isoscape = iso, calibfit = cal, verbose = FALSE))
  expect_false(all(terra::values(ass$group$pv) == 0, na.rm = TRUE))

  ## plots
  expect_no_error(print(plot(iso)))
  expect_no_error(suppressWarnings(print(plot(ass, who = 1:2))))
})

test_that("spaMM Earth distances do not depend on the representation of longitudes", {
  skip_on_cran()
  ## the fits and predictions must be the same whether longitudes are expressed
  ## within [-180, 180] or within [0, 360], so that expressing them within
  ## [0, 360] (as done by isofit() when sources straddle the antimeridian) and
  ## muffling the corresponding spaMM warning is safe
  set.seed(42)
  n <- 40
  d180 <- data.frame(long = c(runif(25, 120, 180), runif(15, -180, -150)), lat = runif(n, -40, 50))
  d180$y <- -2 * abs(d180$lat) + 5 * sin(d180$long %% 360 / 20) + rnorm(n)
  d360 <- d180
  d360$long <- d360$long %% 360
  formula <- y ~ abs(lat) + Matern(1 | long + lat)
  fit180 <- spaMM::fitme(formula, data = d180, control.dist = list(dist.method = "Earth"))
  fit360 <- .muffle_long_warning(spaMM::fitme(formula, data = d360, control.dist = list(dist.method = "Earth")))

  expect_equal(as.numeric(stats::logLik(fit360)), as.numeric(stats::logLik(fit180)), tolerance = 1e-6)
  expect_equal(spaMM::fixef(fit360), spaMM::fixef(fit180), tolerance = 1e-4)
  expect_equal(
    unlist(spaMM::get_ranPars(fit360, which = "corrPars")),
    unlist(spaMM::get_ranPars(fit180, which = "corrPars")),
    tolerance = 1e-4
  )

  new180 <- data.frame(long = c(175, -175, 150, -155, 179.9), lat = c(0, 10, 30, -20, 5))
  new360 <- new180
  new360$long <- new360$long %% 360
  pred180 <- spaMM::predict.HLfit(fit180, newdata = new180, variances = list(predVar = TRUE))
  pred360 <- .muffle_long_warning(spaMM::predict.HLfit(fit360, newdata = new360, variances = list(predVar = TRUE)))
  pred_mixed <- .muffle_long_warning(spaMM::predict.HLfit(fit360, newdata = new180, variances = list(predVar = TRUE)))
  expect_equal(as.numeric(pred360), as.numeric(pred180), tolerance = 1e-4)
  expect_equal(as.numeric(pred_mixed), as.numeric(pred180), tolerance = 1e-4)
  expect_equal(attr(pred360, "predVar"), attr(pred180, "predVar"), tolerance = 1e-4)
})

## getelev() with elevatr mocked (no download) --------------------------------

local_fake_elevatr <- function(env = parent.frame(), offset_west = 0) {
  ## replaces elevatr::get_elev_raster() by a fake function returning a raster
  ## covering the requested bounding box, with the longitude as cell values;
  ## the calls are recorded in the returned environment
  calls <- new.env()
  calls$bbox <- list()
  fake_get_elev_raster <- function(locations, z, prj, clip, ...) {
    expect_true(all(locations$x >= -180 & locations$x <= 180)) ## as required by elevatr
    bbox <- c(min(locations$x), max(locations$x), min(locations$y), max(locations$y))
    calls$bbox[[length(calls$bbox) + 1]] <- bbox
    ## rasters downloaded separately are not necessarily aligned:
    offset <- if (bbox[2] < 0) offset_west else 0
    raster <- terra::rast(
      xmin = bbox[1] + offset, xmax = bbox[2] + offset, ymin = bbox[3], ymax = bbox[4],
      resolution = 0.5, crs = prj
    )
    terra::values(raster) <- terra::crds(raster)[, "x"]
    raster
  }
  testthat::local_mocked_bindings(get_elev_raster = fake_get_elev_raster, .package = "elevatr", .env = env)
  calls
}

test_that("getelev() downloads areas not crossing the antimeridian in one go", {
  skip_if_not_installed("elevatr")
  calls <- local_fake_elevatr()
  file <- withr::local_tempfile(fileext = ".tif")
  suppressMessages(getelev(file, long_min = -10, long_max = 30, lat_min = 0, lat_max = 50, margin_pct = 0, verbose = FALSE))
  expect_length(calls$bbox, 1)
  expect_equal(calls$bbox[[1]], c(-10, 30, 0, 50))
  expect_equal(as.vector(terra::ext(terra::rast(file))), c(xmin = -10, xmax = 30, ymin = 0, ymax = 50))
})

test_that("getelev() downloads areas crossing the antimeridian in two parts", {
  skip_if_not_installed("elevatr")
  for (long_max in c(-150, 210)) { ## two equivalent ways of defining the area
    calls <- local_fake_elevatr()
    file <- withr::local_tempfile(fileext = ".tif")
    suppressMessages(getelev(file, long_min = 70, long_max = long_max, lat_min = -50, lat_max = 70, margin_pct = 0, verbose = FALSE))
    expect_length(calls$bbox, 2)
    expect_equal(calls$bbox[[1]], c(70, 180, -50, 70))
    expect_equal(calls$bbox[[2]], c(-180, -150, -50, 70))
    elev <- terra::rast(file)
    expect_equal(as.vector(terra::ext(elev)), c(xmin = 70, xmax = 210, ymin = -50, ymax = 70))
    expect_false(anyNA(terra::values(elev)))
    ## cells east of 180 contain the data downloaded for the corresponding western cells
    expect_equal(terra::extract(elev, cbind(c(100.25, 200.25), 10.25))[[1]], c(100.25, -159.75))
  }
})

test_that("getelev() applies margins and handles misaligned parts across the antimeridian", {
  skip_if_not_installed("elevatr")
  calls <- local_fake_elevatr(offset_west = 0.1)
  file <- withr::local_tempfile(fileext = ".tif")
  suppressMessages(getelev(file, long_min = 70, long_max = -150, lat_min = -50, lat_max = 70, verbose = FALSE))
  ## 5% of 140 degrees of longitude and of 120 degrees of latitude
  expect_equal(calls$bbox[[1]], c(63, 180, -56, 76))
  expect_equal(calls$bbox[[2]], c(-180, -143, -56, 76))
  elev <- terra::rast(file)
  expect_equal(terra::res(elev), c(0.5, 0.5))
  expect_equal(terra::xmin(elev), 63)
  expect_gte(terra::xmax(elev), 217)
  expect_equal(terra::extract(elev, cbind(200.25, 10.25))[[1]], -159.75, tolerance = 0.2)
})
