## Tests for the detection of spatial extrapolation during calibration (see issue #200)

square <- function(long_center = 0, lat_center = 0, half_width = 10) {
  data.frame(
    long = long_center + c(-1, 1, 1, -1, 0) * half_width,
    lat = lat_center + c(-1, -1, 1, 1, 0) * half_width
  )
}

test_that(".points_outside_hull() works in all four hemispheres", {
  for (sign_long in c(-1, 1)) {
    for (sign_lat in c(-1, 1)) {
      ref <- square(long_center = sign_long * 50, lat_center = sign_lat * 30)
      long <- sign_long * 50 + c(0, 5, -9, 11, 0)
      lat <- sign_lat * 30 + c(0, -5, 9, 0, -20)
      expect_identical(
        .points_outside_hull(ref$long, ref$lat, long, lat),
        c(FALSE, FALSE, FALSE, TRUE, TRUE),
        info = paste("sign_long =", sign_long, "; sign_lat =", sign_lat)
      )
    }
  }
})

test_that(".points_outside_hull() works for a realistic South American design", {
  ref <- data.frame(long = c(-70, -40, -45, -75, -60), lat = c(-5, -5, -35, -40, -20))
  expect_identical(
    .points_outside_hull(ref$long, ref$lat, long = c(-60, -55, -30, -60), lat = c(-20, -15, -20, 0)),
    c(FALSE, FALSE, TRUE, TRUE)
  )
})

test_that(".points_outside_hull() considers locations on the hull boundary as inside", {
  ref <- square()
  ## locations identical to the hull vertices
  expect_false(any(.points_outside_hull(ref$long, ref$lat, ref$long, ref$lat)))
  ## locations on hull edges
  expect_false(any(.points_outside_hull(ref$long, ref$lat, long = c(0, 10, 0, -10), lat = c(-10, 0, 10, 0))))
  ## locations on an oblique edge, subject to floating point errors
  ref2 <- data.frame(long = c(0, 0.3, 0.1), lat = c(0, 0.7, 0.9))
  expect_false(.points_outside_hull(ref2$long, ref2$lat, long = 0.1, lat = 0.7 / 3))
  ## locations just outside
  expect_true(all(.points_outside_hull(ref$long, ref$lat, long = c(0, 10.001), lat = c(-10.001, 0))))
})

test_that(".points_outside_hull() handles hulls straddling the antimeridian", {
  ref <- data.frame(long = c(170, 178, -178, -170, 175), lat = c(-40, -30, -35, -45, -38))
  expect_identical(
    .points_outside_hull(ref$long, ref$lat, long = c(176, 179, -175, 0, 160), lat = c(-38, -36, -40, -38, -38)),
    c(FALSE, FALSE, FALSE, TRUE, TRUE)
  )
  ## same with longitudes expressed within [0, 360)
  expect_identical(
    .points_outside_hull(ref$long %% 360, ref$lat, long = c(176, 179, 185, 0, 160), lat = c(-38, -36, -40, -38, -38)),
    c(FALSE, FALSE, FALSE, TRUE, TRUE)
  )
})

test_that(".points_outside_hull() handles longitudes expressed in different conventions", {
  ref <- square(long_center = -60, lat_center = -20)
  expect_identical(
    .points_outside_hull(ref$long, ref$lat, long = c(-60, 300, 300 + 360), lat = c(-20, -20, -20)),
    c(FALSE, FALSE, FALSE)
  )
  expect_identical(
    .points_outside_hull(ref$long %% 360, ref$lat, long = c(-60, 300), lat = c(-20, -20)),
    c(FALSE, FALSE)
  )
})

test_that(".points_outside_hull() handles degenerate hulls", {
  ## single location (possibly duplicated)
  expect_identical(.points_outside_hull(c(1, 1), c(2, 2), long = c(1, 1.5), lat = c(2, 2)), c(FALSE, TRUE))
  ## aligned locations
  expect_identical(
    .points_outside_hull(long_ref = 1:5, lat_ref = 1:5, long = c(2.5, 5, 6, 2), lat = c(2.5, 5, 6, 3)),
    c(FALSE, FALSE, TRUE, TRUE)
  )
})

test_that(".points_outside_hull() handles missing values", {
  ref <- square()
  expect_identical(
    .points_outside_hull(c(ref$long, NA), c(ref$lat, 5), long = c(0, NA, 20), lat = c(0, 0, 0)),
    c(FALSE, NA, TRUE)
  )
  expect_error(.points_outside_hull(NA_real_, NA_real_, 0, 0))
  expect_error(.points_outside_hull(1:2, 1, 0, 0))
})

## isoscape fit shared by the calibfit() tests below
set.seed(123)
sources_extrapolation <- prepsources(GNIPDataDE[sample(seq_len(nrow(GNIPDataDE)), 50), ])
fit_extrapolation <- isofit(sources_extrapolation)

test_that("calibfit() does not flag calibration sites located at source sites or within their hull", {
  sources <- sources_extrapolation
  fit <- fit_extrapolation

  hull_sources <- sources[grDevices::chull(sources[, c("long", "lat")]), ]
  n <- nrow(hull_sources)
  calib <- CalibDataBat[seq_len(n + 1), ]
  calib$long <- c(hull_sources$long, mean(sources$long))
  calib$lat <- c(hull_sources$lat, mean(sources$lat))
  calib$site_ID <- paste0("site_", seq_len(n + 1))

  expect_no_message(calibfit(calib, fit), message = "extrapolation")

  ## a calibration site far away should be flagged
  calib_out <- calib
  calib_out$long[1] <- max(sources$long) + 5
  expect_message(calibfit(calib_out, fit), regexp = "\\* 1 correspond to locations outside")
})

## builds calibration data with a known number of samples outside the hull of
## the source sites: 9 + 3 samples inside (for the seed used here) and 5 outside
make_calib_count_design <- function(sources) {
  hull_sources <- sources[grDevices::chull(sources[, c("long", "lat")]), ]
  n_hull <- nrow(hull_sources)

  ## samples inside the hull: one per hull vertex (on the boundary), two at the
  ## centroid (same site), and one in the middle of the first hull edge
  long_in <- c(hull_sources$long, rep(mean(sources$long), 2), mean(hull_sources$long[1:2]))
  lat_in <- c(hull_sources$lat, rep(mean(sources$lat), 2), mean(hull_sources$lat[1:2]))
  site_in <- c(paste0("in_", seq_len(n_hull)), rep("in_centroid", 2), "in_edge")

  ## samples outside the hull: three at one site east of the hull, one south of it,
  ## and one just beyond a hull vertex
  long_out <- c(rep(max(sources$long) + 2, 3), mean(sources$long), hull_sources$long[1])
  lat_out <- c(rep(mean(sources$lat), 3), min(sources$lat) - 2, hull_sources$lat[1])
  site_out <- c(rep("out_east", 3), "out_south", "out_vertex")
  ## move the last sample away from the centroid, beyond the first hull vertex
  long_out[5] <- long_out[5] + 0.1 * (hull_sources$long[1] - mean(sources$long))
  lat_out[5] <- lat_out[5] + 0.1 * (hull_sources$lat[1] - mean(sources$lat))

  n_in <- length(long_in)
  n_out <- length(long_out)
  calib <- CalibDataBat[seq_len(n_in + n_out), ]
  calib$long <- c(long_in, long_out)
  calib$lat <- c(lat_in, lat_out)
  calib$site_ID <- c(site_in, site_out)
  calib$elev <- 100 ## same elevation everywhere, so that samples of a site share a location
  rownames(calib) <- NULL
  attr(calib, "n_in") <- n_in
  attr(calib, "n_out") <- n_out
  calib
}

## extracts the message about extrapolation produced by calibfit()
extrapolation_message <- function(calib, fit) {
  msgs <- testthat::capture_messages(calibfit(calib, fit))
  paste(grep("extrapolation", msgs, value = TRUE), collapse = "")
}

test_that("calibfit() counts the number of calibration samples involving extrapolation correctly", {
  sources <- sources_extrapolation
  calib <- make_calib_count_design(sources)
  n_in <- attr(calib, "n_in")
  n_out <- attr(calib, "n_out")

  ## sanity check that the internal helper agrees with the design
  expect_identical(
    .points_outside_hull(sources$long, sources$lat, calib$long, calib$lat),
    rep(c(FALSE, TRUE), c(n_in, n_out))
  )

  msg <- extrapolation_message(calib, fit_extrapolation)
  expect_match(msg, paste("Out of your", n_in + n_out, "calibration samples"), fixed = TRUE)
  expect_match(msg, paste("*", n_out, "correspond to locations outside"), fixed = TRUE)
})

test_that("the test of extrapolation in calibfit() works with tibbles", {
  ## we mimic tibbles as in test_tibbles.R since tibble is not a dependency of IsoriX
  as_fake_tibble <- function(x) {
    class(x) <- c("tbl_df", "tbl", "data.frame")
    x
  }

  calib_df <- make_calib_count_design(sources_extrapolation)
  calib_tbl <- as_fake_tibble(calib_df)
  calib_tbl$site_ID <- as.character(calib_tbl$site_ID)

  sources_tbl <- as_fake_tibble(sources_extrapolation)
  sources_tbl$source_ID <- as.character(sources_tbl$source_ID)
  fit_tbl <- isofit(sources_tbl)

  ref <- extrapolation_message(calib_df, fit_extrapolation) ## df and df
  expect_match(ref, paste("*", attr(calib_df, "n_out"), "correspond to locations outside"), fixed = TRUE)

  expect_identical(extrapolation_message(calib_tbl, fit_extrapolation), ref) ## tbl and df
  expect_identical(extrapolation_message(calib_df, fit_tbl), ref) ## df and tbl
  expect_identical(extrapolation_message(calib_tbl, fit_tbl), ref) ## tbl and tbl
})
