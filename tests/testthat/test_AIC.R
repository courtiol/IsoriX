## Tests for the conditional AIC corrected for the residual dispersion model (#101)

agg <- prepsources(data = GNIPDataDE)

fit_default <- suppressMessages(isofit(data = agg, verbose = FALSE))
fit_disp_elev <- isofit(data = agg, disp_model_fix = list(elev = TRUE), verbose = FALSE)
fit_no_ranef <- isofit(
  data = agg,
  mean_model_rand = list(uncorr = FALSE, spatial = FALSE),
  disp_model_rand = list(uncorr = FALSE, spatial = FALSE),
  verbose = FALSE
)

test_that("AIC() adds twice the number of parameters of the residual dispersion model to the cAIC of the mean model", {
  cAIC_mean <- spaMM::get_any_IC(fit_default$mean_fit, verbose = FALSE, short.names = TRUE)[["cAIC"]]
  ## default residual dispersion model: intercept + 2 variances + 2 Matern parameters (nu & rho)
  expect_equal(AIC(fit_default), unname(cAIC_mean) + 2 * 5)
})

test_that("the number of parameters is counted correctly for different model structures", {
  res <- AIC(fit_default, fit_disp_elev, fit_no_ranef)
  expect_s3_class(res, "data.frame")
  expect_identical(rownames(res), c("fit_default", "fit_disp_elev", "fit_no_ranef"))
  expect_identical(colnames(res), c("K_mean", "K_disp", "cAIC_uncorrected", "cAIC"))
  expect_equal(res$K_mean, c(5, 5, 1))
  expect_equal(res$K_disp, c(5, 6, 1))
  expect_equal(res$cAIC, res$cAIC_uncorrected + 2 * res$K_disp)
  expect_equal(res["fit_default", "cAIC"], AIC(fit_default))
})

test_that("the nugget parametrisation leads to the same number of parameters", {
  fit_nugget <- suppressMessages(isofit(
    data = agg,
    uncorr_terms = list(mean_model = "nugget", disp_model = "nugget"),
    verbose = FALSE
  ))
  res <- AIC(fit_default, fit_nugget)
  expect_equal(res$K_mean, c(5, 5))
  expect_equal(res$K_disp, c(5, 5))
})

test_that("AIC() sums the corrected cAIC over all pairs of models for MULTIISOFIT objects", {
  agg_monthly <- prepsources(data = GNIPDataDE, month = 1:2, split_by = "month")
  multifit <- isomultifit(
    data = agg_monthly,
    mean_model_rand = list(uncorr = FALSE, spatial = FALSE),
    disp_model_rand = list(uncorr = FALSE, spatial = FALSE),
    verbose = FALSE
  )
  expect_equal(
    AIC(multifit),
    sum(vapply(multifit$multi_fits, AIC, numeric(1)))
  )
  res <- AIC(multifit, multifit)
  expect_equal(res$K_disp, c(2, 2))
})

test_that("AIC() checks its inputs", {
  expect_error(AIC(fit_default, k = log(10)), "must be equal to 2")
  expect_error(AIC(fit_default, fit_default$mean_fit), "created by isofit\\(\\) or isomultifit\\(\\)")
})
