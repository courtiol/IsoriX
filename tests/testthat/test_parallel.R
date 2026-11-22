## Tests for the parallel computation of isomultifit() and isomultiscape() (#52)

test_that("the argument Ncpu is checked", {
  expect_identical(.check_Ncpu(2), 2L)
  expect_error(.check_Ncpu(0), "single positive integer")
  expect_error(.check_Ncpu(1.5), "single positive integer")
  expect_error(.check_Ncpu(c(1, 2)), "single positive integer")
  expect_error(.check_Ncpu(NA), "single positive integer")
  expect_error(isomultifit(data = prepsources(data = GNIPDataDE, month = 1:2, split_by = "month"), Ncpu = 0), "single positive integer")
})

test_that("tasks run sequentially when a single CPU is used", {
  res <- .lapply_IsoriX(list(1, 2), function(x, shared) x + shared$y, shared = list(y = 10), Ncpu = 1)
  expect_identical(res, list(11, 12))
})

test_that("what workers print or signal is captured", {
  fn <- function(x, shared) {
    print("printed")
    message("a message")
    warning("a warning")
    x
  }
  res <- .run_in_worker(1, task_fn = fn)
  expect_identical(res$value, 1)
  expect_identical(res$output, '[1] "printed"')
  expect_length(res$conditions, 2)
  expect_s3_class(res$conditions[[1]], "message")
  expect_s3_class(res$conditions[[2]], "warning")
  expect_null(res$conditions[[2]]$call)
  err <- .run_in_worker(1, task_fn = function(x, shared) stop("boom"))
  expect_s3_class(err$value, "error")
})

test_that("terra objects are packed and unpacked", {
  packed <- .pack_terra(ElevRasterDE)
  expect_s4_class(packed, "PackedSpatRaster")
  expect_equal(terra::values(.unpack_terra(packed)), terra::values(ElevRasterDE))
  expect_identical(.pack_terra(1), 1)
  expect_identical(.unpack_terra(1), 1)
})

test_that("parallel computations give the same results as sequential ones", {
  skip_on_cran() ## starting R sessions is slow and uses more CPUs
  agg_monthly <- prepsources(data = GNIPDataDE, month = 1:2, split_by = "month")
  args <- list(
    data = agg_monthly,
    mean_model_fix = list(elev = TRUE),
    mean_model_rand = list(uncorr = FALSE, spatial = FALSE),
    disp_model_rand = list(uncorr = FALSE, spatial = FALSE),
    verbose = FALSE
  )
  fit_seq <- do.call(isomultifit, c(args, Ncpu = 1))
  fit_par <- do.call(isomultifit, c(args, Ncpu = 2))
  expect_s3_class(fit_par, "MULTIISOFIT")
  expect_identical(names(fit_par$multi_fits), names(fit_seq$multi_fits))
  expect_equal(AIC(fit_par), AIC(fit_seq))
  expect_equal(
    lapply(fit_par$multi_fits, function(f) spaMM::fixef(f$mean_fit)),
    lapply(fit_seq$multi_fits, function(f) spaMM::fixef(f$mean_fit))
  )

  scape_seq <- suppressMessages(isomultiscape(raster = ElevRasterDE, isofit = fit_seq, Ncpu = 1, verbose = FALSE))
  msgs <- capture_messages(
    scape_par <- isomultiscape(raster = ElevRasterDE, isofit = fit_par, Ncpu = 2, verbose = FALSE)
  )
  expect_match(msgs, "predictions", all = FALSE) ## messages produced in the workers are displayed
  expect_s3_class(scape_par, "ISOSCAPE")
  expect_s4_class(scape_par$isoscapes, "SpatRaster")
  expect_s4_class(scape_par$sp_points$sources, "SpatVector")
  expect_equal(terra::values(scape_par$isoscapes), terra::values(scape_seq$isoscapes))
  expect_equal(terra::geom(scape_par$sp_points$sources), terra::geom(scape_seq$sp_points$sources))
})

test_that("errors produced in the workers are propagated", {
  skip_on_cran() ## starting R sessions is slow and uses more CPUs
  fn <- function(x, shared) if (x == 2) stop("boom in task ", x) else x
  expect_error(.lapply_IsoriX(list(1, 2), fn, Ncpu = 2), "boom in task 2")
})
