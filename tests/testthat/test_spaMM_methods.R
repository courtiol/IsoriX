## Tests for the consistency between the different spaMM fitting methods used by isofit()

test_that("isofit() gives the same fits with HLfit and fitme in the absence of Matern random effects", {
  agg <- prepsources(data = GNIPDataDE)
  args <- list(
    data = agg,
    mean_model_fix = list(elev = TRUE, lat_abs = TRUE),
    disp_model_fix = list(elev = TRUE),
    mean_model_rand = list(uncorr = TRUE, spatial = FALSE),
    disp_model_rand = list(uncorr = TRUE, spatial = FALSE),
    verbose = FALSE
  )
  fit_fitme <- do.call(isofit, c(args, list(spaMM_method = list(mean_model = "fitme", disp_model = "fitme"))))
  fit_HLfit <- do.call(isofit, c(args, list(spaMM_method = list(mean_model = "HLfit", disp_model = "HLfit"))))

  for (model in c("disp_fit", "mean_fit")) {
    ## the residual variance of the residual dispersion model must be fixed (to 2), not estimated
    expect_equal(spaMM::DoF(fit_HLfit[[model]]), spaMM::DoF(fit_fitme[[model]]))
    expect_equal(spaMM::fixef(fit_HLfit[[model]]), spaMM::fixef(fit_fitme[[model]]), tolerance = 1e-5)
    expect_equal(
      spaMM::get_ranPars(fit_HLfit[[model]], which = "lambda"),
      spaMM::get_ranPars(fit_fitme[[model]], which = "lambda"),
      tolerance = 1e-5
    )
    expect_equal(c(logLik(fit_HLfit[[model]])), c(logLik(fit_fitme[[model]])), tolerance = 1e-5)
  }
  expect_identical(spaMM::DoF(fit_HLfit$disp_fit)[["p_rdisp"]], 0)
  expect_equal(AIC(fit_HLfit), AIC(fit_fitme), tolerance = 1e-5)
})
