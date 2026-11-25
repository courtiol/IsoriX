## The variances of the random effects of the calibration fit must enter the
## variance of the assignment test (#235)

test_that(".calib_ranef_var() returns the variances of uncorrelated random effects (lab)", {
  set.seed(1)
  n_sites <- 12
  d <- data.frame(
    site_ID = factor(rep(seq_len(n_sites), each = 6)),
    species_ID = factor(rep(LETTERS[1:6], times = n_sites))
  )
  d$source_value <- rep(stats::runif(n_sites, -80, -40), each = 6)
  d$sample_value <- 3 + 0.5 * d$source_value +
    stats::rnorm(n_sites, sd = 2)[d$site_ID] + stats::rnorm(6, sd = 3)[d$species_ID] +
    stats::rnorm(nrow(d))

  ## species_rand is not (yet) exposed by calibfit(), so we call the internal fitting function
  calib <- .calibfit_lab(data = .prepare_data_calib(d, method = "lab"), species_rand = TRUE, verbose = FALSE)
  vc <- spaMM::VarCorr(calib$calib_fit)
  expect_setequal(names(calib$ranef_var), c("site_ID", "species_ID"))
  expect_equal(calib$ranef_var[["site_ID"]], vc$Variance[vc$Group == "site_ID"])
  expect_equal(calib$ranef_var[["species_ID"]], vc$Variance[vc$Group == "species_ID"])

  ## no random effect
  calib_no_site <- .calibfit_lab(data = d[, c("source_value", "sample_value")], species_rand = FALSE, verbose = FALSE)
  expect_length(calib_no_site$ranef_var, 0)
})

test_that("calibration methods without random effects store no ranef_var", {
  calib_desk <- suppressMessages(calibfit(
    data = data.frame(intercept = 1, slope = 0.5, intercept_se = 1, slope_se = 0.1, resid_var = 5),
    method = "desk", verbose = FALSE
  ))
  expect_length(calib_desk$ranef_var, 0)
  expect_false(any(grepl("random effects", capture.output(summary(calib_desk)))))
})

test_that("isofind() adds the site variance of the calibration fit to term 2 (#235)", {
  german <- fixture_german() ## see helper-fixtures.R
  scape <- german$scape

  set.seed(2)
  aliens <- create_aliens(
    calib_fn = list(intercept = 3, slope = 0.5, resid_var = 2),
    isoscape = scape, raster = ElevRasterDE, n_sites = 15, min_n_samples = 5, max_n_samples = 5
  )
  site_effect <- stats::rnorm(nlevels(aliens$site_ID), sd = 2)
  aliens$sample_value <- aliens$sample_value + site_effect[as.integer(aliens$site_ID)]
  calib <- suppressMessages(calibfit(data = aliens, method = "lab", verbose = FALSE))
  expect_named(calib$ranef_var, "site_ID")
  expect_gt(calib$ranef_var[["site_ID"]], 0)
  expect_true(any(grepl("random effects", capture.output(summary(calib)))))

  assignment <- suppressWarnings(isofind(
    data = AssignDataAlien[1:2, ], isoscape = scape, calibfit = calib, mask = NULL, verbose = FALSE
  ))

  ## variance of the test statistic for the first sample, computed by hand
  slope <- calib$param[["slope"]]
  X <- c(1, (AssignDataAlien$sample_value[1] - calib$param[["intercept"]]) / slope)
  expected <- terra::values(scape$isoscapes$mean_predVar)[, 1] + # term 1
    (calib$phi + calib$ranef_var[["site_ID"]]) / slope^2 + # term 2
    drop(X %*% calib$fixefCov %*% X) / slope^2 # term 3
  expect_equal(terra::values(assignment$sample$stat_var[[1]])[, 1], expected)

  ## calibration fits created by former versions of IsoriX do not store ranef_var
  calib_old <- calib
  calib_old$ranef_var <- NULL
  assignment_old <- suppressWarnings(isofind(
    data = AssignDataAlien[1:2, ], isoscape = scape, calibfit = calib_old, mask = NULL, verbose = FALSE
  ))
  expect_equal(terra::values(assignment_old$sample$stat_var), terra::values(assignment$sample$stat_var))
})

test_that("the corrMatrix term of the calibration method wild is not part of ranef_var (#235)", {
  skip_on_cran()
  german <- fixture_german() ## see helper-fixtures.R
  calib <- suppressMessages(calibfit(data = CalibDataAlien, isofit = german$fit, verbose = FALSE))
  expect_named(calib$ranef_var, "site_ID")
  ## the first lambda is the one of corrMatrix(1|site_ID), fixed to slope^2
  expect_equal(calib$ranef_var[["site_ID"]], unname(calib$calib_fit$lambda[2]))
  expect_true(any(grepl("corrMatrix(1|site_ID) + (1|site_ID)", capture.output(print(calib)), fixed = TRUE)))
})
